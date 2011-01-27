/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/tablemanager/sqllogiclist.h,v $
* Author:       $Author: boon $
* Locker:       $Locker:  $
* State:        $State: Exp $
* Revision:     $Revision: 1.1 $
*
* History:      Use the CVS command log to display revision history
*               information.
*
* Description:
*
* Notes:
*
******************************************************************************/
#ifndef _SQLLOGICLIST_H
#define _SQLLOGICLIST_H

#include "../libs/sql/sqltable.h"
#include <map>

class CSqlLogicList : private SqlTable
{

public: 

	CSqlLogicList();

	bool GetLogic(uint32_t _nLogicID, CMyStr* _strRes);
    bool GetLogicName( uint32_t _nLogicID, CMyStr* _strRes);

private:

	typedef	std::map<unsigned int, CMyStr> TMapLogic;

	TMapLogic m_mapLogic;
};

#endif
