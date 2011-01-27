/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/chatserver/chatserver.cpp,v $
* Author:       $Author: win $
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

#include "chtserver.h"
#include "../libs/server/selector.h"
#include "../libs/server/connector.h"

int main(int argc, char *argv[])
{
	CHTServer* pCHTServer = CHTServer::Instance();

	Connector Connector( static_cast<ISocketManager*>( pCHTServer ) );

//	if( !Connector.Connect( 1234, "82.146.42.150", static_cast<AccessInfo*>( pCHTServer ) ) )
	if( !Connector.Connect( 1234, "localhost", static_cast<AccessInfo*>( pCHTServer ) ) )
		return -1;

	Selector::Instance()->StartLoop();

	Connector.Close( SHUT_RDWR );

	Selector::KillObject();

	CHTServer::KillObject();

	return 0;
}
