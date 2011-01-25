/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/tools/mystr.h,v $
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
#ifndef _MYSTR_H_

#define _MYSTR_H_



#include <string>
#include <vector>
#include <iostream>

#include "structs.h"



class CMyStr : public std::string

{

public:

	

	CMyStr();

	CMyStr( int );

	CMyStr( const char* );
    
    CMyStr( TVecChar * );

	CMyStr( std::string );

	virtual ~CMyStr();



	CMyStr	operator+( CMyStr& ) const;	

};


typedef std::vector<const CMyStr*> TVecMyStr;

CMyStr		Int2Str( int _nVal );



#endif

