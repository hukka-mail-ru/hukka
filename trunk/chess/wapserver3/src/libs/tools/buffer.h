/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/tools/buffer.h,v $
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

#ifndef _BUFFER_H
#define _BUFFER_H

class Buffer
{
public:

	Buffer();
	~Buffer();

	char*	GetDataStart();
	char*	GetDataEnd();

	int		DataSize() const;
	int		FreeSize() const;

	void		IncBuffer();
	void		AddDataSize( int _nSize );

	void		RemoveData( char* );
private:

	char*	m_pBuff;
	int		m_nBuffSize;
	int		m_nDataSize;
};

#endif
