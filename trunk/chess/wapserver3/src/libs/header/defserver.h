/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/header/defserver.h,v $
* Author:       $Author: boon $
* Locker:       $Locker:  $
* State:        $State: Exp $
* Revision:     $Revision: 1.3 $
*
* History:      Use the CVS command log to display revision history
*               information.
*
* Description:
*
* Notes:
*
******************************************************************************/

#ifndef _DEFSERVER_H
#define _DEFSERVER_H

//Server


// services (from wsUsers table)
const int SOC  =                        0;
const int SRV  =                        1;
const int REG  =                        2;
const int CHAT =                        3;
const int TBM  =                        4; // Table Manager
const int CHS  =                        6; // Chess Server

// game logic ids (see tbLogicList table)
const int LOGIC_ID_GAMMON =              1;
const int LOGIC_ID_CHESS  =              2;
/*

#define	SOC		0
#define	SRV		1
#define	REG		2
#define TBM		3
#define LXO		4
*/

// See the wsUsers table. This value is the initial counter value, so "common" users have IDs beginning with this value.
const int WS_USERS_AUTO_INCREMENT_OFFSET =  100;

#endif
