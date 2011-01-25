/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/sql/sqlbase.cpp,v $
* Author:       $Author: leha $
* Locker:       $Locker:  $
* State:        $State: Exp $
* Revision:     $Revision: 1.6 $
*
* History:      Use the CVS command log to display revision history
*               information.
*
* Description:
*
* Notes:
*
******************************************************************************/

#include "sqlbase.h"

#include <iostream>
//#include <syslog.h>

CSqlBase::CSqlBase():
	m_isConnected( false ),
	m_cszLogin(DB_LOGIN),
	m_cszPassword(DB_PASSWORD),
	m_cszDBName(DB_NAME)
{
	Init();
}

CSqlBase::CSqlBase(const char* _cszLogin, const char* _cszPassword, const char* _cszDBName):
	m_isConnected( false ),
	m_cszLogin(_cszLogin),
	m_cszPassword(_cszPassword),
	m_cszDBName(_cszDBName)

{
	Init();
}


void CSqlBase::Init()
{
    m_isConnected = false;

	mysql_init( &m_MySQL );

	Connect();

}

CSqlBase::~CSqlBase()
{
	mysql_close( &m_MySQL );
}

MYSQL_RES* CSqlBase::Query( const char* _pcQuery )
{
	mysql_query( &m_MySQL, _pcQuery );
	MYSQL_RES* pRes = mysql_store_result( &m_MySQL );

	return pRes;
}

MYSQL_RES* CSqlBase::QueryReal( const char* _pcQuery, unsigned long length) 
{
	mysql_real_query( &m_MySQL, _pcQuery, length);
	MYSQL_RES* pRes = mysql_store_result( &m_MySQL );

	return pRes;
}

void CSqlBase::FreeRes( MYSQL_RES* _pRes )
{
	mysql_free_result( _pRes );
}

bool CSqlBase::Connect()
{
	//syslog( LOG_INFO | LOG_LOCAL0, "CSqlBase::Connect()" );
	//syslog( LOG_INFO | LOG_LOCAL0, m_cszLogin );
	//syslog( LOG_INFO | LOG_LOCAL0, m_cszPassword );
	
	if( mysql_real_connect( &m_MySQL, 0, m_cszLogin, m_cszPassword, 0, 0, 0, 0 ) )
		m_isConnected = mysql_select_db( &m_MySQL, m_cszDBName ) == 0 ;
	
/*	if ( m_isConnected )
		syslog( LOG_INFO | LOG_LOCAL0, "CSqlBase::Connect() true" );
	else
		syslog( LOG_INFO | LOG_LOCAL0, "CSqlBase::Connect() false" );*/
	
	return m_isConnected;
}

int CSqlBase::LastInsertId()
{
	return mysql_insert_id(&m_MySQL);
}
