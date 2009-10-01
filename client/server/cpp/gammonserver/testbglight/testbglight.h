/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/testbglight/testbglight.h,v $
* Author:       $Author: boon $
* Locker:       $Locker:  $
* State:        $State: Exp $
* Revision:     $Revision: 1.4 $
*
* History:      Use the CVS command log to display revision history
*               information.
*
* Description:
*
* Notes:
*
******************************************************************************/
#ifndef _TESTBGLIGHT_H
#define _TESTBGLIGHT_H

void ShowPos(CGammonLogic& _GammonLogic);

void ShowRes(const TVByte* _data);

void LoadPos(char* szFileName, CGammonLogic& _GammonLogic);

void SavePos(char* szFileName, CGammonLogic& _GammonLogic);

#endif