/******************************************************************************
*
*       (C) Copyright 2009, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/cpp/chatserver/chatdefs.h $
* Author:       $Author: leha $
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

#ifndef CHATDEFS_H
#define CHATDEFS_H

const int       COMMON_CHAT_ID = 0;

// CHAT SERVER MESSAGES 20 -
const int       CMD_CHAT_MSG            = 20;
const int       CMD_CHAT_JOIN           = 21;
const int       CMD_CHAT_LEAVE          = 22;
const int       CMD_CHAT_DELETE_HISTORY = 23;
const int       ANS_CHAT_MSG            = 24;
const int       ANS_CHAT_USER_ONLINE    = 25;
const int       ANS_CHAT_USER_JOINED    = 26;
const int       ANS_CHAT_USER_LEFT      = 27;



#endif
