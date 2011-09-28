/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/tools/structs.h,v $
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
#ifndef _STRUCTS_H
#define _STRUCTS_H

#include <vector>
#include <strstream>

typedef unsigned int uint32_t;

typedef std::vector<char> TVecChar;
typedef std::vector<char> TVecByte;
typedef std::vector<unsigned int> TVecUINT;

struct SOutMsg
{
    char m_btCmd;
    char* m_arData;
};

#pragma pack(1)
struct STbmCmd
{
	char		m_chCmd;
	uint32_t	m_nTableID;
	char		m_chData;
	uint32_t    m_Parameter;
};

#endif
