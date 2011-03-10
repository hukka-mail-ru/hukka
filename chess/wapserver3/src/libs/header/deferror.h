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

const int		NOERR		= 0;

//Parser Msg
const int		ERRNOSIGN	= 1;
const int		ERRVER		= 2;
const int		ERRTOBIG	= 3;
const int		ERRCRC		= 4;
const int		ERRNOEND	= 5;

//SRV command 1
const int		ERRBADLOGIN	= 1;
const int		ERRBADPASS	= 2;
const int		ERRUSERONLINE	= 3;

//REG command 1
const int		ERRLOGINEXIST	= 3;

//SQLerror

const int 	    ERRUNDEF	= 127;
const int		ERRNOACCESSTO	= 128;
const int		ERRCOMMAND	= 129;
const int		ERRMSG		= 130;
const int		ERRNOADDR	= 131;

#endif
