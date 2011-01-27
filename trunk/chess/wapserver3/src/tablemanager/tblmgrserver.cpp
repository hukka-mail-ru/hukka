#include "tblmgrserver.h"
#include "../libs/header/defserver.h"
#include "../libs/header/deferror.h"
#include "../libs/sql/sqltableusers.h"
#include "../libs/sql/sqlratingtable.h"
#include "tbmdefs.h"

#include <iostream>
#include <stdint.h>
#include <syslog.h>

#define MYDEBUG

CTblMgrServer* CTblMgrServer::m_pSelf = 0;
int CTblMgrServer::m_nRefCount = 0;

CTblMgrServer* CTblMgrServer::Instance()
{
	if( m_pSelf == 0 )
		m_pSelf = new CTblMgrServer;

	++m_nRefCount;

	return m_pSelf;
}

void CTblMgrServer::FreeInst()
{
	--m_nRefCount;

	if( m_nRefCount > 0 )
		return;

	KillObject();
}

void CTblMgrServer::KillObject()
{
	if( m_pSelf != 0 )
		delete m_pSelf;

	m_pSelf = 0;
	m_nRefCount = 0;
}

CTblMgrServer::CTblMgrServer()
	:AccessInfo( "tbm", "*96CD954E820BAB0F2C7D96E90A6D59B2CF4B8A96" )
{

}

CTblMgrServer::~CTblMgrServer()
{

}

void CTblMgrServer::DoAllMsg( MySocket* _pSocket )
{
	setSocket( _pSocket );
	ClientMsg inMsg;

	while( _pSocket->GetMsg( inMsg ) )
	{
		newMsg( &inMsg );
	}
}


/*!
    \fn CTblMgrServer::setSocket( MySocket * _pSocket )
 */
void CTblMgrServer::setSocket( MySocket * _pSocket )
{
	m_pSocket = _pSocket;
}


/*!
    \fn CTblMgrServer::newMsg( ClientMsg* _pMsg )
 */
void CTblMgrServer::newMsg( ClientMsg* _pMsg )
{
	TVecChar vecCmd;
	_pMsg->GetData( ClientMsg::etpCommand, &vecCmd );

#ifdef MYDEBUG
	std::cout << "--- INCOMING MSG --- FROM: " << ( uint32_t )_pMsg->GetTo() <<
	             ";  COMMAND: " << ( uint32_t ) _pMsg->GetCommand() <<
	             ";  DATA: ";
    for ( int i = 0; i < vecCmd.size(); ++i )
    {
        std::cout << (int) vecCmd[i] << " ";
    }
    std::cout << std::endl;

#endif
	if ( _pMsg->GetTo() == SRV )
	{
		if ( _pMsg->GetCommand() == 1 )
			if ( vecCmd[0] != NOERR )
			{
				syslog( LOG_INFO | LOG_LOCAL0, "connection to socketserver failed" );
				/// @todo Закончить приложение
			}
			else
			    syslog( LOG_INFO | LOG_LOCAL0, "started" );

	}

	if ( _pMsg->GetTo() < 100 )
		return;
	//std::cerr <<


	switch( _pMsg->GetCommand() )
	{
		case CMD_CREATE:
		{
			Create( _pMsg->GetTo(), &vecCmd );
			break;
		}
        case CMD_GET_PARAMS:
        {
            GetParams( _pMsg->GetTo(), &vecCmd );
            break;
        }
		case CMD_RANDOM_OP:
		{
		    Random( _pMsg->GetTo(), &vecCmd );
			break;
		}
		case CMD_FIND:
		{
			Find( _pMsg->GetTo(), &vecCmd );

			break;
		}
        case CMD_DELETE:
        {
            if (vecCmd.size() != 2*sizeof(uint32_t))
            {
                return;
            }

            uint32_t *pTableID = (uint32_t*) &vecCmd[sizeof(uint32_t)];

            DeleteTable(_pMsg->GetTo(), vecCmd[0], *pTableID);
            break;
        }
        case CMD_SET_PARAMS:
        {
            break;
        }
		case CMD_GETMYTBL:
		{
			uint32_t *pLogicID = (uint32_t*) &vecCmd[0];

			TVecUINT vec;

			STbmCmd sMsg;
			sMsg.m_chCmd = ANS_MYTBL;

			if ( ! m_TbmCommands.GetMyTable( *pLogicID, _pMsg->GetTo(), &vec ) )
			{
              sMsg.m_nTableID = *pLogicID;
              sMsg.m_chData = ST_NOTVALID;
			}
			else
			{
				sMsg.m_nTableID = vec.back();
                sMsg.m_chData = ST_VALID;
			}

			sendMsg( _pMsg->GetTo(), &sMsg, sizeof (sMsg ) );

//			cmdLoose( _pMsg->GetTo(), *nTableID );

			break;
		}
		default:
			break;
	}
}

void CTblMgrServer::Create( uint32_t _nUserID, const TVecChar* _vecData )
{
	uint32_t pLogicID = ( uint32_t ) _vecData->at(0);
	int nVecSize = _vecData->size();

	TVecPrms vec;
	STbmCmd sMsg;
	uint32_t nParamID, nParam;
	CMyStr strPassword = "";
	sMsg.m_chCmd = ANS_CREATE;
	TbmCommands::CrRes Result;

	if ( (nVecSize > sizeof(uint32_t)) && (nVecSize - sizeof(uint32_t)) <= sizeof(uint32_t) )
	{
		sMsg.m_nTableID = pLogicID;
		sMsg.m_chData = ST_NOTVALID;
	}
	else
	{
	    for ( int i = sizeof( uint32_t ); i < nVecSize; i += 2*sizeof( uint32_t ) )
	    {
		    if ( i >= ( nVecSize - sizeof( uint32_t ) ) )
		    {
			    Result = TbmCommands::NVPAR;
			    break;
		    }
		    nParamID = (uint32_t) _vecData->at(i);

		    if ( nParamID == m_TbmCommands.GetPasswordID() )
		    {

			    strPassword = (char*)_vecData->at(i+sizeof(uint32_t));
			    break;
		    }
		    else
		    {
			    nParam = *(uint32_t*) &_vecData->at(i+sizeof(uint32_t));
		    }

		    vec.push_back(std::make_pair<int,int>(nParamID, nParam));

	    }

	    Result = m_TbmCommands.Create( pLogicID, _nUserID, vec, &strPassword );

	    switch ( Result )
	    {
		    case TbmCommands::DONE:
			    sMsg.m_nTableID = m_TbmCommands.LastInsertId();
			    sMsg.m_chData = ST_VALID;
			    break;
		    case TbmCommands::TABEX:
		    case TbmCommands::NVPAR:
			    sMsg.m_nTableID = pLogicID;
			    sMsg.m_chData = ST_NOTVALID;
			    break;
	    }
	}

	sendMsg( _nUserID, &sMsg, sizeof (sMsg ) );
}


void CTblMgrServer::Find( uint32_t _nUserID, const TVecChar* _vecData )
{
#ifdef MYDEBUG
    std::cout << "CTblMgrServer::Find()" << std::endl;
#endif
    uint32_t pLogicID = ( uint32_t ) _vecData->at(0);
    uint32_t nCount = (uint32_t) _vecData->at(sizeof(uint32_t));
#ifdef MYDEBUG
    std::cout << "CTblMgrServer::Find()" << std::endl;
    std::cout << "LogicID: " << CMyStr(pLogicID) << std::endl;
    std::cout << "Count: " << CMyStr(nCount) << std::endl;
    std::cout << "_vecData size: " << CMyStr(_vecData->size()) << std::endl;
#endif
	int nVecSize = _vecData->size();

	TVecFindPrms vec;
    TVecUINT vecRes;
    TVecUINT vecMsgData;
    CSendedMsg sendedMsg;
    sendedMsg.addData((char)ANS_TABLE);

    int nParam, nValue, nCond, nLogic;


    if ( (_vecData->size() - sizeof(uint32_t)*2)%(sizeof(uint32_t)*4) != 0 )
    {
        vecMsgData.push_back(pLogicID);
        vecMsgData.push_back(0);

        sendedMsg.addData(&vecMsgData);

        sendMsg( _nUserID, &sendedMsg );
        return;
    }

    for ( int i = sizeof(uint32_t)*2; i < _vecData->size(); i += sizeof(uint32_t)*4 )
    {
        SFindParameters params;
        params.m_nParameter = *(int*) &_vecData->at(i);
        params.m_nValue = *(int*) &_vecData->at(i+sizeof(int));
        params.m_nCondition = *(int*) &_vecData->at(i+sizeof(int)*2);
        params.m_nLogic = *(int*) &_vecData->at(i+sizeof(int)*3);

        vec.push_back(params);
   }

   bool bRes = m_TbmCommands.Find( pLogicID, _nUserID, nCount, &vec, &vecRes);

#ifdef MYDEBUG
    std::cout << "Find return " << CMyStr(vecRes.size()) << " values" << std::endl;
    for (TVecUINT::const_iterator it = vecRes.begin(); it != vecRes.end(); ++it)
    {
        std::cout << (uint32_t)*it << " ";
    }
    std::cout << std::endl;
#endif

    if ( !bRes )
    {
#ifdef MYDEBUG
        std::cout << "Find return FALSE" << std::endl;
#endif
        vecMsgData.push_back(pLogicID);
        vecMsgData.push_back(0);

        sendedMsg.addData(&vecMsgData);

        sendMsg( _nUserID, &sendedMsg );
        return;
    }


    vecMsgData.push_back(pLogicID);
    std::copy(vecRes.begin(), vecRes.end(), std::back_inserter(vecMsgData));

    sendedMsg.addData(&vecMsgData);

    sendMsg( _nUserID, &sendedMsg );

}

void CTblMgrServer::Random( uint32_t _nUserID, const TVecChar* _vecData )
{

	uint32_t pLogicID = ( uint32_t ) _vecData->at(0);
#ifdef MYDEBUG
    std::cout << "CTblMgrServer::Random()" << std::endl;
    std::cout << "LogicID: " << CMyStr(pLogicID) << std::endl;
    std::cout << "_vecData size: " << CMyStr(_vecData->size()) << std::endl;
#endif
	int nVecSize = _vecData->size();

	TVecFindPrms vec;
    TVecUINT vecMsgData;
    CSendedMsg sendedMsg;
    sendedMsg.addData((char)ANS_RANDOM_OP);

    int nParam, nValue, nCond, nLogic;


    if ( (_vecData->size() - sizeof(uint32_t))%(sizeof(uint32_t)*4) != 0 )
    {
        vecMsgData.push_back(pLogicID);
        vecMsgData.push_back(0);

        sendedMsg.addData(&vecMsgData);

        sendMsg( _nUserID, &sendedMsg );
        return;
    }

    uint32_t nRes;

    if ( _vecData->size() > sizeof(uint32_t) )
    {

        for ( int i = sizeof(uint32_t); i < _vecData->size(); i += sizeof(uint32_t)*4 )
        {
            SFindParameters params;
            params.m_nParameter = *(int*) &_vecData->at(i);
            params.m_nValue = *(int*) &_vecData->at(i+sizeof(int));
            params.m_nCondition = *(int*) &_vecData->at(i+sizeof(int)*2);
            params.m_nLogic = *(int*) &_vecData->at(i+sizeof(int)*3);

            vec.push_back(params);
        }

        nRes = m_TbmCommands.RandomOpponent( pLogicID, _nUserID, &vec);
    }
    else
    {
        nRes = m_TbmCommands.RandomOpponent( pLogicID, _nUserID );
    }

#ifdef MYDEBUG
    std::cout << "RandomOpponent return " << CMyStr(nRes) << std::endl;
#endif

    vecMsgData.push_back(pLogicID);
    vecMsgData.push_back(nRes);

    sendedMsg.addData(&vecMsgData);

    sendMsg( _nUserID, &sendedMsg );

}

void CTblMgrServer::GetParams( uint32_t _nUserID, const TVecChar* _vecData )
{

    uint32_t pLogicID = ( uint32_t ) _vecData->at(0);

    CMyStr strLogicTable;
    CSendedMsg sendedMsg;
    TVecUINT vecMsgData, vecParamIDs;
    TVecPrms vecPrms;

    if ( !m_TbmCommands.getSqlLogicList()->GetLogicName(pLogicID, &strLogicTable) )
    {
        sendedMsg.addData(pLogicID);
        sendedMsg.addData((char)ST_NOTVALID);

        sendMsg( _nUserID, &sendedMsg );
        return;
    }

    strLogicTable = "tb" + strLogicTable + "Rating";

    CSqlRatingTable sqlRatingTable( strLogicTable.c_str(), 0 );

    sendedMsg.addData((char)ANS_GET_PARAMS);

    if ( (_vecData->size() < sizeof(uint32_t)*3) || (_vecData->size()%sizeof(uint32_t) != 0) )
    {
        sendedMsg.addData(pLogicID);
        sendedMsg.addData((char)ST_NOTVALID);

        sendMsg( _nUserID, &sendedMsg );
        return;
    }

    uint32_t nTableID = *(uint32_t*) &_vecData->at(sizeof(uint32_t));

    for ( int i = sizeof(uint32_t)*2; i < _vecData->size(); i+=sizeof(uint32_t) )
    {
        vecParamIDs.push_back( (uint32_t) _vecData->at(i) );
    }

    if ( !m_TbmCommands.GetTableParams(pLogicID, nTableID, vecParamIDs, &vecPrms) )
    {
        sendedMsg.addData(pLogicID);
        sendedMsg.addData((char) ST_NOTVALID);

        sendMsg( _nUserID, &sendedMsg );
        return;
    }

    sendedMsg.addData((uint32_t) nTableID );
    sendedMsg.addData((char)ST_VALID);

    for ( int i = 0; i < vecPrms.size(); ++i )
    {
        if ( vecPrms.at(i).first >= 2)
        {
            sendedMsg.addData( (uint32_t)vecPrms.at(i).first );
            sendedMsg.addData( (uint32_t)vecPrms.at(i).second );
        }

        else
        {
            TVecChar vecChar;
            SqlTableUsers wsUsers;

            if ( wsUsers.GetUserName( vecPrms.at(i).second, &vecChar ) )
            {

#ifdef MYDEBUG
                std::cout << "CTblMgrServer::GetParams sqlRatingTable.getRating()" << std::endl;
#endif

                uint32_t nRating = sqlRatingTable.getRating( vecPrms.at(i).second );

                sendedMsg.addData( (uint32_t)vecPrms.at(i).first );
                sendedMsg.addData( &vecChar );
                sendedMsg.addData( (char) 0 );
                sendedMsg.addData( nRating );
            }
        }

    }

    sendMsg( _nUserID, &sendedMsg );

}

void CTblMgrServer::sendMsg( uint32_t _nTo, CSendedMsg *_pMsg )
{
    ClientMsg Msg;
    TVecChar vecCmd = _pMsg->getDataVec();
#ifdef MYDEBUG
    for (TVecChar::const_iterator it = vecCmd.begin(); it != vecCmd.end(); ++it)
    {
      std::cout << (uint32_t) *it << " ";
    }
    std::cout << std::endl;
#endif
    Msg.InitMsg(_nTo, vecCmd);
    m_pSocket->AddMsg(Msg);
}
/*!
    \fn CTblMgrServer::sendMsg( uint32_t _nTo, void* _pMsg, int _nSize )
 */
void CTblMgrServer::sendMsg( uint32_t _nTO, void* _pMsg, int _nSize )
{
	TVecChar vecCmd;

	vecCmd.assign( (char*)_pMsg, (char*)(_pMsg) + _nSize );
#ifdef MYDEBUG
    std::cout << "sendMsg data: ";
    for (int i = 0; i < _nSize; ++i)
    {
        std::cout << (uint32_t)*((char*)_pMsg+i) << " ";
    }
    std::cout << std::endl;
	//syslog( LOG_INFO | LOG_LOCAL0, "Send to %d CMD: %d TableID: %d Param: %d",_nTO, (int)(( STbmCmd* ) _pMsg)->m_chCmd,(( STbmCmd* ) _pMsg)->m_nTableID, (int)(( STbmCmd* ) _pMsg)->m_chData );
    for (TVecChar::const_iterator it = vecCmd.begin(); it != vecCmd.end(); ++it)
    {

        std::cout << (uint32_t) *it << " ";
    }
    std::cout << std::endl;
#endif

	ClientMsg Msg;

	Msg.InitMsg( _nTO, vecCmd );

	m_pSocket->AddMsg( Msg );

}

void CTblMgrServer::SetParams( uint32_t _nUserID, const TVecChar *_vecData )
{
}

void CTblMgrServer::DeleteTable( uint32_t _nUserID, uint32_t _nLogicID, uint32_t _nTableID )
{

#ifdef MYDEBUG
    std::cout << "CTblMgrServer::DeleteTable() _nTableID = " << _nTableID << std::endl;
#endif

    CSendedMsg sendedMsg;

    sendedMsg.addData((char)ANS_DELETE);
    sendedMsg.addData(_nLogicID);
    sendedMsg.addData(_nTableID);

    if (!m_TbmCommands.Delete(_nLogicID, _nUserID, _nTableID))
    {
        sendedMsg.addData((char)ST_NOTVALID);
    }
    else
    {
        sendedMsg.addData((char)ST_VALID);
    }

    sendMsg( _nUserID, &sendedMsg );

}
