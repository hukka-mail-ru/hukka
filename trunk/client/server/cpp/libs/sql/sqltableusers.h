/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/sql/sqltableusers.h,v $
* Author:       $Author: boon $
* Locker:       $Locker:  $
* State:        $State: Exp $
* Revision:     $Revision: 1.2 $
*
* History:      Use the CVS command log to display revision history
*               information.
*
* Description:
*
* Notes:
*
******************************************************************************/

#ifndef _SQLTABLEUSERS_H
#define _SQLTABLEUSERS_H

#include "sqltable.h"
#include "../tools/structs.h"

class CSqlTableUsers : private CSqlTable
{
public:

	CSqlTableUsers();
	~CSqlTableUsers();

	char		IsUserReg( char*, char*, uint32_t& );
	char		DoUserReg( char*, char*, uint32_t& );
private:

	bool		GetPass( char*, TVecChar* );
	bool		GetID( char*, uint32_t& );
	void		Insert( char*, char* );
	bool		CheckCharSet( const char* ) const;
};

#endif
