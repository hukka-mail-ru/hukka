/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/sql/sqltable.cpp,v $
* Author:       $Author: leha $
* Locker:       $Locker:  $
* State:        $State: Exp $
* Revision:     $Revision: 1.15 $
*
* History:      Use the CVS command log to display revision history
*               information.
*
* Description:
*
* Notes:
*
******************************************************************************/

#include "sqltable.h"

#include <syslog.h>

/*
const char* g_cszDefDbLogin = 0;

const char* g_cszDefDbPassword = 0;

const char* g_cszDefDbName = 0;*/


CSqlTable::CSqlTable(const char* _cTableName, const char* _cTableStruct )
	:m_strTableName( _cTableName )
{
	CMyStr strQuery( "CREATE TABLE IF NOT EXISTS "+m_strTableName+" ("+_cTableStruct+")" );

	m_sqlBase.FreeRes( m_sqlBase.Query( strQuery.c_str() ) );
}

CSqlTable::CSqlTable( const char* cszLogin, const char* cszPassword, const char* cszDBName, const char* _cTableName ) :
     m_sqlBase(cszLogin, cszPassword, cszDBName ),
     m_strTableName( _cTableName )
{
}

CSqlTable::CSqlTable( const char* _cTableName ) : 
	 m_strTableName( _cTableName )
{
}

void CSqlTable::Open( const char* _cTableName ) 
{
    m_strTableName = _cTableName;
}


CSqlTable::~CSqlTable()
{

}
/* SELECT <_pcFlt> FROM <m_strTableName> WHERE <_pcKey> ='< _pcVal>' */
bool CSqlTable::SelectToStr(const char* _pcFlt, const char* _pcKey,const char* _pcVal, TVecChar* _pvecRes )
{
	bool	isRes;

	CMyStr strQuery( "SELECT "+CMyStr( _pcFlt )+" FROM "+m_strTableName+" WHERE "+CMyStr( _pcKey )+"='"+CMyStr( _pcVal )+"'" );

	MYSQL_RES* pRes = m_sqlBase.Query( strQuery.c_str() );

	//syslog( LOG_INFO | LOG_LOCAL0, strQuery.c_str() );

	if( isRes = ( pRes != 0 ) )
	{
		MYSQL_ROW Row = mysql_fetch_row( pRes );

		unsigned long *lengths = mysql_fetch_lengths(pRes);

		if( isRes = (( Row != 0 ) && *lengths != 0) )
			_pvecRes->assign( Row[0], Row[0] + lengths[0] );
	}

	m_sqlBase.FreeRes( pRes );
	
/*	int isnRes;
	if ( isRes )
		isnRes = 1;
	else
		isnRes = 0;

	syslog( LOG_INFO | LOG_LOCAL0, "CSqlTable::SelectToStr return %d", isnRes );*/
	
	return isRes;
}

/* SELECT <_pcFlt> FROM <m_strTableName> WHERE <_pcKey> ='< _pcVal>' */
bool CSqlTable::Select(const char* _pcFlt, const char* _pcKey,const char* _pcVal, TTable* _pTable )
{
    CMyStr strQuery;

    strQuery = "SELECT "+CMyStr( _pcFlt )+" FROM "+m_strTableName+" WHERE "+CMyStr( _pcKey );

    if (_pcVal != 0)
    	strQuery = strQuery + "='" + CMyStr( _pcVal ) + "'" ;
    else
        strQuery = strQuery + " IS NULL";


	MYSQL_RES* pRes = m_sqlBase.Query( strQuery.c_str() );

	if (pRes == 0)
		return false;
	
	CopyToTable(_pTable, pRes);
	
	m_sqlBase.FreeRes( pRes );

	return true;
}

/* SELECT <_pcFlt> FROM <m_strTableName> WHERE <_pcKey> */
bool CSqlTable::Select(const char* _pcFlt, const char* _pcKey, TTable* _pTable )
{
    CMyStr strQuery;

    strQuery = "SELECT "+CMyStr( _pcFlt )+" FROM "+m_strTableName+" WHERE "+CMyStr( _pcKey );

	MYSQL_RES* pRes = m_sqlBase.Query( strQuery.c_str() );

	if (pRes == 0)
		return false;
	
	CopyToTable(_pTable, pRes);
	
	m_sqlBase.FreeRes( pRes );

	return true;
}

void CSqlTable::CopyToTable(TTable* _pTable, MYSQL_RES* pRes)
{
	unsigned int nNumFields = mysql_num_fields(pRes);

	TRow vecStrRow(nNumFields);

	MYSQL_ROW Row;

	while ( Row = mysql_fetch_row(pRes) )
	{
		unsigned long *lengths;
		lengths = mysql_fetch_lengths(pRes);

		_pTable->push_back(vecStrRow);

		for(TRow::iterator it = _pTable->back().begin(); it !=  _pTable->back().end(); ++it)
		{
			if ( Row[it - _pTable->back().begin()] != 0 )
				*it = CMyStr(Row[it - _pTable->back().begin()]);
		}
	}

}

void CSqlTable::Insert( const CMyStr* strCol, const CMyStr* strVal )
{
	TVecMyStr vecCol(1);
	TVecMyStr vecVal(1);

	vecCol.at(0) = strCol; 
	vecVal.at(0) = strVal; 

	Insert(vecCol, vecVal);

}

/* INSERT INTO <m_strTableName> VALUE (<_cszVal>) */
void CSqlTable::Insert( const char* _cszVal )
{
	CMyStr strQuery( "INSERT INTO "+m_strTableName+" VALUE ("+CMyStr( _cszVal )+")" );

	MYSQL_RES* pRes = m_sqlBase.Query( strQuery.c_str() );

	m_sqlBase.FreeRes( pRes );
}

/* INSERT INTO <m_strTableName> (_vecCols[0] ... _vecCols[n])  VALUES (_vecValues[0] ... _vecValues[n]) */
bool CSqlTable::Insert(const TVecMyStr& _vecCols, const TVecMyStr& _vecValues )
{
	if (_vecCols.size() != _vecValues.size() )
		return false;

	CMyStr strQuery( "INSERT INTO " + m_strTableName + "(");
	//+ " VALUES (" + CMyStr( _pcValue ) + ")" );

	for(TVecMyStr::const_iterator it = _vecCols.begin(); it != _vecCols.end(); ++it )
		strQuery = strQuery + **it + 
		(( (it - _vecCols.begin()) != (_vecCols.size() - 1) ) ? "," : "");

	strQuery = strQuery + ") VALUES (";

	for(TVecMyStr::const_iterator itVal = _vecValues.begin(); itVal != _vecValues.end(); ++itVal )
		strQuery = strQuery + **itVal +
		(( (itVal - _vecValues.begin()) != (_vecValues.size() - 1) ) ? "," : "");

	strQuery = strQuery + ")";

	//std::cout << strQuery.c_str();

	MYSQL_RES* pRes = m_sqlBase.Query( strQuery.c_str() );

	m_sqlBase.FreeRes( pRes );

	return true;
}

/* UPDATE <m_strTableName> SET  <_cszCol>=<_cszNewVal> WHERE <_cszKey> =< _cszVal>;*/
void CSqlTable::Update( const char* _cszCol, const char* _cszNewVal, const char* _cszKey, const char* _cszVal ) 
{
	CMyStr strQuery( "UPDATE " + m_strTableName + " SET "+ _cszCol + "=" + _cszNewVal + 
		            " WHERE "+ _cszKey +"="+_cszVal );

	MYSQL_RES* pRes = m_sqlBase.Query( strQuery.c_str() );

	m_sqlBase.FreeRes( pRes );
}

void CSqlTable::ar2blob(const TVecByte& _cvecIn, CMyStr* _pRes)
{
    std::strstream str;
    str << "0x"; 

    for(TVecByte::const_iterator it = _cvecIn.begin(); it !=_cvecIn.end();  ++it)
	{
        char buf[3];
        sprintf(buf, "%02X", (int)*(it) );
        str << buf;
	}

    str.rdbuf()->sputc((char)0); 

	*_pRes = str.rdbuf()->str();
}
