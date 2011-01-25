/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/sql/sqlbase.h,v $
* Author:       $Author: leha $
* Locker:       $Locker:  $
* State:        $State: Exp $
* Revision:     $Revision: 1.7 $
*
* History:      Use the CVS command log to display revision history
*               information.
*
* Description:
*
* Notes:
*
******************************************************************************/

#ifndef _SQLBASE_H
#define _SQLBASE_H

#ifdef WIN32
#include <windows.h>
//#include <my_global.h>
#endif

#include <mysql.h>
#include "../tools/mystr.h"
#include "../tools/structs.h"

#define DB_LOGIN		"WapServer3"

#define DB_PASSWORD		"win74"

#define DB_NAME			"WapServer3DB"

class CSqlBase
{
public:

	CSqlBase();

	CSqlBase(const char* cszLogin, const char* cszPassword, const char* cszDBName);

	~CSqlBase();

	MYSQL_RES*	Query( const char* );
	
	MYSQL_RES*  QueryReal( const char* _pcQuery, unsigned long length);

	void		FreeRes( MYSQL_RES* );

	int LastInsertId();

private:

	bool		Connect();

	void        Init();

private:

	MYSQL		m_MySQL;

	bool		m_isConnected;

	const char* m_cszLogin;

	const char* m_cszPassword;

	const char* m_cszDBName;
};

#endif
