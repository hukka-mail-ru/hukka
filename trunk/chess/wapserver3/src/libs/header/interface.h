/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/header/interface.h,v $
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

#ifndef _INTERFACE_H
#define _INTERFACE_H

#include <vector>

class ClientMsg;
class MySocket;

class IReaderWriter
{
public:

	virtual void		DoRead() = 0;
	virtual void		DoWrite() = 0;
};

class ISender
{
public:

	virtual void		AddMsg( const ClientMsg& ) = 0;
};

class ISocketManager
{
public:

	virtual void		RemoveSocket( MySocket* ) = 0;
	virtual void		AddInMsg( MySocket* ) = 0;
	virtual void		AddOutMsg( MySocket* ) = 0;

	virtual void       onSocketClosed( MySocket* ) = 0;
};

typedef std::vector<char> TVByte;

class IGameLogic
{

public:

	enum StepRes  { NotValid /*= 0*/,  Valid, Win, Loose, Draw, TimeOut };

	virtual bool SetPos(const TVByte& _vecbtPos) = 0;		// Установить позицию на доске.

	virtual IGameLogic::StepRes StepAnl( TVByte* _pvecbtPos ) = 0;

	virtual const TVByte* GetPosForDB() = 0;					// Получить текущую плзицию

    virtual const TVByte* GetPosForClient() = 0;
};


#endif
