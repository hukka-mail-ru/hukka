/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/tablemanager/tablemanager.cpp,v $
* Author:       $Author: leha $
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

#define MYDEBUG

#include <iostream>
#include <syslog.h>

#include "../libs/server/selector.h"
#include "../libs/server/connector.h"

#include "tblmgrserver.h"
#include "testtbm/tmctest.h"

int StartService()
{

	openlog( "tablemanager", LOG_CONS, LOG_LOCAL0 );

	CTblMgrServer* pTblMgrServer = CTblMgrServer::Instance();

	Connector Connector( static_cast<ISocketManager*>( pTblMgrServer ) );

	if( !Connector.Connect( 1234, "localhost", static_cast<AccessInfo*>( pTblMgrServer ) ) )
	{
		std::cerr << "server not found" << std::endl;
		return -1;
	}

	Selector::Instance()->StartLoop();

	Connector.Close( SHUT_RDWR );

	Selector::KillObject();

	CTblMgrServer::KillObject();

	return 0;
}

void StartUTests()
{
	TMCTest utests;

	utests.Run();
}

int main(int argc, char *argv[])
{
	if ( argc > 1 )
	{
		std::string param( argv[1] );
		if (param == "-u" )
		{
			StartUTests();
		}
	}
	else
	{
		StartService();
	}

	return 0;
}

