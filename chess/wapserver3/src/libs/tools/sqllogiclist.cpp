/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/tablemanager/sqllogiclist.cpp,v $
* Author:       $Author: boon $
* Locker:       $Locker:  $
* State:        $State: Exp $
* Revision:     $Revision: 1.5 $
*
* History:      Use the CVS command log to display revision history
*               information.
*
* Description:
*
* Notes:
*
******************************************************************************/

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <stdlib.h>
#include "sqllogiclist.h"

CSqlLogicList::CSqlLogicList():
SqlTable(DB_LOGIN, DB_PASSWORD, DB_NAME, "tbLogicList")
{
	TTable tbl;

	/* load all table : SELECT * FROM tstSelect WHERE 1 = 1 */
	Select("*","1","1", &tbl);

#ifdef MYDEBUG
	std::cout << "LOGIC LIST:" << std::endl;
#endif

	for(TTable::const_iterator it = tbl.begin(); it != tbl.end(); ++it)
	{
		m_mapLogic[atoi(it->at(0).c_str())] = it->at(1).c_str();
#ifdef MYDEBUG
	std::cout << "logic :"<< it->at(1).c_str() << " id: " << atoi(it->at(0).c_str()) << std::endl;
#endif

	}

}

bool CSqlLogicList::GetLogic(unsigned int _nLogicID, CMyStr* _strRes)
{
	TMapLogic::const_iterator res = m_mapLogic.find(_nLogicID);

#ifdef MYDEBUG
	std::cout << "LOGIC LIST:" << std::endl;
#endif

	if (res != m_mapLogic.end())
	{
		*_strRes = "tb" + res->second + "TableList";
		return true;
	}
	else
		return false;
}

bool CSqlLogicList::GetLogicName( uint32_t _nLogicID, CMyStr* _strRes)
{
    TMapLogic::const_iterator res = m_mapLogic.find(_nLogicID);

    if (res != m_mapLogic.end())
    {
        *_strRes = res->second;
        return true;
    }
    else
        return false;
}
