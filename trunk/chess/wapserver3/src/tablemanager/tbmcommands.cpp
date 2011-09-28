#include "../libs/header/defparams.h"
#include "../libs/sql/sqltable.h"
#include "../libs/sql/sqlaccounttable.h"
#include "../header/defservice.h"
#include "tbmcommands.h"
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#include <algorithm>

#define MYDEBUG

#define RANDOM_BUFFER 100
#define TABLE_OPEN 1

/*
 * 1 - TABLE HAS BEEN CREATED
 * 2 - WAITING FOR START OF THE GAME
 * 3 - GAME HAS STARTED
 *
 * 7 - DELETED ?
 */


TbmCommands::TbmCommands()
: m_nLastId(0)
{
	SqlTable paramsTable("tbParamList");
	CMyStr strWhere = "isPassword != 0";
	TTable tbl;
	paramsTable.Select("ParamId", strWhere.c_str(), &tbl);

	if ( !tbl.empty() )
	{
		m_nPasswordID = atoi(tbl.begin()->at(0).c_str());
	}

}

TbmCommands::~TbmCommands()
{

}

TbmCommands::CrRes TbmCommands::CheckParams(const TVecPrms& params, uint32_t playerID)
{
	CMyStr strWhere;
	TTable tbl;

	SqlTable paramsTable("tbParamList");

	for (TVecPrms::const_iterator i = params.begin(); i != params.end(); ++i)
	{

		strWhere = "ParamID = " + CMyStr(i->first);
		paramsTable.Select("*", strWhere.c_str(), &tbl);

        if ( tbl.empty() )
			return TbmCommands::NVPAR;

		if ( ( tbl.begin()->at(4) != "0" ) )
			continue;

		// IsPlayerID should be 0
		if ( tbl.begin()->at(5) != "0" )
		    return TbmCommands::NVPAR;

		// the value should be in range: MIN ... MAX
		// if MAX = 0 => no constrains.
		if (  i->second < atoi(tbl.begin()->at(2).c_str()) )// val < MIN ?
		    return TbmCommands::TOOSMALL;

		 // MAX exists AND val > MAX ?
        if ( atoi(tbl.begin()->at(3).c_str()) != 0 &&  i->second > atoi(tbl.begin()->at(3).c_str()) )
            return TbmCommands::TOOLARGE;

        tbl.clear();
	}

	// Check balance
	SqlAccountTable accountTable;
	for(int i=0; i<params.size(); i++)
	{
	    if(params[i].first == PARAMETER_ID_BET &&
	       accountTable.getBalance(playerID) < params[i].second)
	    {
	        return TbmCommands::NOBALANCE;
	    }
	}


	return TbmCommands::DONE;
}

bool TbmCommands::GetLogicTable(int _nLogicID, SqlTable* _pRes)
{
	CMyStr strLogicTable;

	if ( m_sqlLogicList.GetLogic(_nLogicID, &strLogicTable) )
	{
		_pRes->Open(strLogicTable.c_str());

        	return true;
    	}
	else
		return false;

}

TbmCommands::CrRes TbmCommands::Create(uint32_t _nLogicID, uint32_t _nPlayerID,
					 const TVecPrms &_vecPrms, const CMyStr* _strPswd )
{
	if (GetMyTable(_nLogicID, _nPlayerID, 0))
	{
#ifdef MYDEBUG
	    	std::cout << "TbmCommands::Create return false! (GetMyTable return true)" << std::endl;
#endif
		return TABEX;
	}

	SqlTable sqlLogicTable("");

	TTable tbl;


	if ( GetLogicTable(_nLogicID, &sqlLogicTable) )
	{

		if ( !_vecPrms.empty() )
		{

		    TbmCommands::CrRes res = CheckParams( _vecPrms, _nPlayerID );
			if ( res != TbmCommands::DONE )
            {
#ifdef MYDEBUG
                std::cout << "Not valid parameters" << std::endl;
#endif
				return res;
            }

			SqlTable paramsTable("tbParamList");
            std::vector<CMyStr> strField, strValue;
            TVecMyStr fields, values;

            strField.push_back(CMyStr("IDPlayer0"));
            strValue.push_back(CMyStr(_nPlayerID));


			for ( TVecPrms::const_iterator i = _vecPrms.begin(); i != _vecPrms.end(); ++i )
			{
                strValue.push_back(CMyStr(i->second));
                CMyStr strWhere = "ParamID = " + CMyStr(i->first);
				paramsTable.Select("ParamName", strWhere.c_str(), &tbl);
                strField.push_back(tbl.begin()->at(0));
                tbl.clear();
			}

            for ( int i = 0; i < strField.size(); ++i )
            {
                fields.push_back(&strField.at(i));
                values.push_back(&strValue.at(i));
#ifdef MYDEBUG
                std::cout << "Field: " << *fields.at(i) << std::endl << "Value: " << *values.at(i) << std::endl;
#endif
            }

            if (!sqlLogicTable.Insert(fields, values))
            {
#ifdef MYDEBUG
        	    std::cout << "TbmCommands::Create return false! (GetLogicTable return false)" << std::endl;
#endif
                return DBERR;
            }
		    m_nLastId = sqlLogicTable.LastInsertId();
#ifdef MYDEBUG
    	    std::cout << "TbmCommands::Create : create table ID: "  << m_nLastId << std::endl;
#endif
            return DONE;

		}

		sqlLogicTable.Insert( &CMyStr("IDPlayer0"), &CMyStr(_nPlayerID)  );

		m_nLastId = sqlLogicTable.LastInsertId();

#ifdef MYDEBUG
	    std::cout << "TbmCommands::Create : create table ID: "  << m_nLastId << std::endl;
#endif
        return DONE;
	}
	else
	{
#ifdef MYDEBUG
	    std::cout << "TbmCommands::Create return false! (GetLogicTable return false)" << std::endl;
#endif
		return NVPAR;
	}
}

bool TbmCommands::Find(uint32_t _nLogicID, uint32_t _nPlayerID, uint32_t _nCount, const TVecFindPrms* _pvecPrms, TVecUINT* _pvecRes)
{
    TVecPrms vecPrms;

#ifdef MYDEBUG
            std::cout << "TbmCommands::Find _pvecPrms->size() = " << CMyStr(_pvecPrms->size()) << std::endl;
#endif
    if (!_pvecPrms)
    {
        if ( !GetAllTables(_nLogicID, _pvecRes) )
        {
            return false;
        }
        else
        {
            return _pvecRes->size() > 1;
        }
    }

    for (int i = 0; i < _pvecPrms->size(); ++i)
    {
#ifdef MYDEBUG
            std::cout << "Param: " << CMyStr(_pvecPrms->at(i).m_nParameter) << std::endl;
            std::cout << "Value: " << CMyStr(_pvecPrms->at(i).m_nValue) << std::endl;
            std::cout << "Cond: " << CMyStr(_pvecPrms->at(i).m_nCondition) << std::endl;
            std::cout << "Logic: " << CMyStr(_pvecPrms->at(i).m_nLogic) << std::endl;
#endif

        if ((_pvecPrms->at(i).m_nCondition > 2)||(_pvecPrms->at(i).m_nLogic > 2))
        {
#ifdef MYDEBUG
            std::cout << "TbmCommands::Find - Not valid condition or logic parameter" << std::endl;
#endif
            return false;
        }

        vecPrms.push_back(std::make_pair<int,int>(_pvecPrms->at(i).m_nParameter, _pvecPrms->at(i).m_nValue));

    }

    TbmCommands::CrRes res = CheckParams( vecPrms, _nPlayerID );
    if (  res != TbmCommands::DONE )
    {
#ifdef MYDEBUG
        std::cout << "TbmCommands::Find - Not valid parameters" << std::endl;
#endif
        return false;
    }
#ifdef MYDEBUG
        std::cout << "TbmCommands::Find - Valid parameters" << std::endl;
#endif

    TTable tbl;
    SqlTable sqlLogicTable("");
    SqlTable paramsTable("tbParamList");

    if ( GetLogicTable(_nLogicID, &sqlLogicTable) )
    {
        TTable tbl;

        CMyStr strWhere = " State < 2 AND (";

        for (int i = 0; i < _pvecPrms->size(); ++i)
        {
            CMyStr strCond, strLogic;

            switch (_pvecPrms->at(i).m_nCondition)     //TODO create CondConvertor
            {
                case 0:
                    strCond = ">";
                    break;
                case 1:
                    strCond = "=";
                    break;
                case 2:
                    strCond = "<";
                    break;
                default:
                    strCond = "";
            }

            switch (_pvecPrms->at(i).m_nLogic)     //TODO create LogicConvertor
            {
                case 1:
                    strLogic = " OR ";
                    break;
                case 2:
                    strLogic = " AND ";
                    break;
                default:
                    strLogic = "";
            }

            CMyStr strWhere1 = "ParamID = " + CMyStr(_pvecPrms->at(i).m_nParameter);
#ifdef MYDEBUG
            std::cout << "TbmCommands::Find() strWhere: " << strWhere1 << std::endl;
#endif
            paramsTable.Select("ParamName", strWhere1.c_str(), &tbl);

            strWhere = strWhere + tbl.begin()->at(0) + strCond + CMyStr(_pvecPrms->at(i).m_nValue);

            if (strLogic == "")
            {
                strWhere += ")";
            }
            else
            {
                strWhere = strWhere + strLogic + " ";
            }

            tbl.clear();


        }

#ifdef MYDEBUG
        std::cout << "TbmCommands::Find() strWhere: " << strWhere << std::endl;
#endif
        sqlLogicTable.Select("TableID", strWhere.c_str(), &tbl);

        if ( tbl.size() == 0 )
        {
#ifdef MYDEBUG
            std::cout << "TbmCommands::Find() tables not founded" << std::endl;
#endif
            return false;
        }
        else
        {
            std::cout << "TbmCommands::Find() db return " << CMyStr(tbl.size()) << " tables" << std::endl;
        }



        if (_pvecRes)
        {

            if (tbl.size() < _nCount )
            {
                _pvecRes->push_back(tbl.size());
            }
            else
            {
                _pvecRes->push_back(_nCount);
            }

            int n = 0;

            for(TTable::const_iterator it = tbl.begin(); it != tbl.end(); ++it)
            {
                if ( (++n) > _nCount )
                    break;
                uint32_t nTableID = atoi(it->at(0).c_str());

                    _pvecRes->push_back(nTableID);
            }

        }
        return _pvecRes->size() > 1;
    }
    else
    {
#ifdef MYDEBUG
        std::cout << "TbmCommands::Find() !GetLogicTable" << std::endl;
#endif
        return false;
    }
}

bool TbmCommands::Delete(uint32_t _nLogicID, uint32_t _nPlayerID, uint32_t _nTableID)
{

    SqlTable sqlLogicTable("");
    CMyStr strWhere;
    TTable tbl;

    if ( GetLogicTable(_nLogicID, &sqlLogicTable) )
    {
        CMyStr strKey = "TableID";
        CMyStr strTableId = CMyStr(_nTableID);

        sqlLogicTable.Delete(strKey.c_str(), strTableId.c_str());

    }
    return true;
}

bool TbmCommands::FindEmpty(int _nLogicID, int _nPlayerID, TVecUINT* vecRes )
{

    SqlTable sqlLogicTable("");

	if ( GetLogicTable(_nLogicID, &sqlLogicTable) )
	{
        	TTable tbl;

		CMyStr strWhere = "IDPlayer1 IS NULL AND State = " + CMyStr(ST_OPEN) + " AND IDPlayer0 <> " + CMyStr(_nPlayerID);

		sqlLogicTable.Select("TableID", strWhere.c_str(), &tbl);

        	vecRes->resize(tbl.size());

       		for(TTable::const_iterator it = tbl.begin(); it != tbl.end(); ++it)
	    	{
            		vecRes->at( it - tbl.begin() ) = atoi(it->at(0).c_str());
	    	}

        	return true;
    	}
	else
	{
		return false;
	}
}

bool TbmCommands::GetTableParams(int _nLogicID, int _nTableID, const TVecUINT& _vecParIDs, TVecPrms *_pvecPrms)
{
	SqlTable paramsTable("tbParamList");
	SqlTable sqlLogicTable("");
	CMyStr strWhere;
	TTable tbl1, tbl2;

	if (!_pvecPrms)
		return false;

	if ( GetLogicTable(_nLogicID, &sqlLogicTable) )
	{
		for (TVecUINT::const_iterator i = _vecParIDs.begin(); i != _vecParIDs.end(); ++i)
		{
			tbl1.clear();
            tbl2.clear();

			strWhere = "ParamID = " + CMyStr(*i);
		    	paramsTable.Select("ParamName", strWhere.c_str(), &tbl1);

		    	if ( tbl1.empty() )
	            		return false;

			strWhere = "TableID = " + CMyStr(_nTableID);
			sqlLogicTable.Select(tbl1.begin()->at(0).c_str(), strWhere.c_str(), &tbl2);

			if (!tbl2.empty())
			{
                int value = atoi(tbl2.begin()->at(0).c_str());
				_pvecPrms->push_back(std::make_pair<int,int>(*i, value));
            }
		}

	}

	return true;
}

bool TbmCommands::GetAllTables( uint32_t _nLogicID, TVecUINT* _pvecRes )
{
    SqlTable sqlLogicTable("");

	if ( GetLogicTable(_nLogicID, &sqlLogicTable) )
	{
		TTable tbl;

		CMyStr strWhere = " State < " + CMyStr(ST_FULL);

		sqlLogicTable.Select("TableID", strWhere.c_str(), &tbl);

		if (_pvecRes)
		{
			_pvecRes->resize(tbl.size());

       			for(TTable::const_iterator it = tbl.begin(); it != tbl.end(); ++it)
			{
				_pvecRes->at( it - tbl.begin() ) = atoi(it->at(0).c_str());
			}

		}
		return tbl.size() > 0;
	}
	else
	{
		return false;
	}
}

bool  TbmCommands::GetMyTable(int _nLogicID, int _nPlayerID, TVecUINT* vecRes)
{
	///TODO change the query to database with using tbParamList and 'isPlayerID' field
    	SqlTable sqlLogicTable("");

	if ( GetLogicTable(_nLogicID, &sqlLogicTable) )
	{
		TTable tbl;

		CMyStr strWhere = "(IDPlayer0 = " + CMyStr(_nPlayerID) +
			           " OR IDPlayer1 = " + CMyStr(_nPlayerID) + ")";

        /* find  :
		SELECT TableID FROM tb<Logic>LogicTable	WHERE (IDPlayer0 = _nPlayerID OR IDPlayer1 = _nPlayerID)*/
		sqlLogicTable.Select("TableID", strWhere.c_str(), &tbl);

		if (vecRes)
		{
			vecRes->resize(tbl.size());

       			for(TTable::const_iterator it = tbl.begin(); it != tbl.end(); ++it)
			{
				vecRes->at( it - tbl.begin() ) = atoi(it->at(0).c_str());
			}

		}
		return tbl.size() > 0;
	}
	else
	{
		return false;
	}

}

int TbmCommands::GetOwner(int _nLogicID, int _nPlayerID)
{
    int owner = 0;
    SqlTable sqlLogicTable("");

    if ( GetLogicTable(_nLogicID, &sqlLogicTable) )
    {
        TTable tbl;

        CMyStr strWhere = "(IDPlayer0 = " + CMyStr(_nPlayerID) +
                       " OR IDPlayer1 = " + CMyStr(_nPlayerID) + ")";

        /* find :
        SELECT TableID FROM tb<Logic>LogicTable WHERE  (IDPlayer0 = _nPlayerID OR IDPlayer1 = _nPlayerID)*/
        sqlLogicTable.Select("IDPlayer0", strWhere.c_str(), &tbl);

        if (!tbl.empty())
        {

            owner = atoi(tbl[0][0].c_str());
        }
    }

    return owner;
}

uint32_t TbmCommands::LastInsertId()
{
    return m_nLastId;
}

uint32_t TbmCommands::RandomOpponent(uint32_t _nLogicID, uint32_t _nPlayerID, const TVecFindPrms* _pvecPrms )
{

    TVecUINT vecPlayerTables, vecResult, vecTmp;
    GetMyTable( _nLogicID, _nPlayerID, &vecPlayerTables );

    if (!_pvecPrms)
    {
        if ( !GetAllTables(_nLogicID, &vecResult) )
        {
            return 0;
        }
    }
    else if ( ! Find( _nLogicID, _nPlayerID, RANDOM_BUFFER, _pvecPrms, &vecResult ) )
    {
        return 0;
    }

    for ( int j = 0; j < vecResult.size(); ++j )
    {
        if ( std::find(vecPlayerTables.begin(), vecPlayerTables.end(), vecResult.at(j)) == vecPlayerTables.end() )
        {
            vecTmp.push_back( vecResult.at(j) );
        }
    }

    if ( vecTmp.size() == 0 )
    {
        return 0;
    }

	srand( (unsigned)time( NULL ) );
	return vecTmp.at( rand() % vecTmp.size() );

}

