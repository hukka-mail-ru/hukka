/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/header/deferror.h,v $
* Author:       $Author: win $
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

#ifndef _DEFERROR_H
#define _DEFERROR_H

#define		NOERR		0

//Parser Msg
#define		ERRNOSIGN	1
#define		ERRVER		2
#define		ERRTOBIG	3
#define		ERRCRC		4
#define		ERRNOEND	5

//SRV command 1
#define		ERRBADLOGIN	1
#define		ERRBADPASS	2
#define		ERRUSERONLINE	3	

//REG command 1
#define		ERRLOGINEXIST	3

//SQLerror

#define 	ERRUNDEF	127
#define		ERRNOACCESSTO	128
#define		ERRCOMMAND	129
#define		ERRMSG		130
#define		ERRNOADDR	131

#endif
