/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/tools/mythread.h,v $
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
#ifndef _MS_MY_THREAD_H_
#define _MS_MY_THREAD_H_

#include "pthread.h"

class MyThread
{
public:
	
	MyThread();
	virtual ~MyThread();

	pthread_t		GetID() const;

	virtual int	StartLoop();
	virtual void	StopLoop();

	virtual int	Run() = 0;
protected:

	int			Continue();
private:

	pthread_t		m_hThread;
};

#endif

