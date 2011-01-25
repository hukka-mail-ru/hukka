/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/libs/sql/sqltable.h,v $
* Author:       $Author: boon $
* Locker:       $Locker:  $
* State:        $State: Exp $
* Revision:     $Revision: 1.12 $
*
* History:      Use the CVS command log to display revision history
*               information.
*
* Description:
*
* Notes:
*
******************************************************************************/

#ifndef _SQLTABLE_H
#define _SQLTABLE_H

#include "sqlbase.h"

typedef std::vector<CMyStr> TRow;

typedef std::vector<TRow> TTable; 


class CSqlTable
{
public:

	CSqlTable( const char* _cTableName, const char* _cTableStruct);

	CSqlTable( const char* cszLogin, const char* cszPassword, const char* cszDBName, const char* _cTableName );

	CSqlTable( const char* _cTableName );

	~CSqlTable();

    void Open( const char* _cTableName );

	/* SELECT <_pcFlt> FROM <m_strTableName> WHERE <_pcKey> ='< _pcVal>' */
	bool SelectToStr( const char* _pcFlt, const char* _pcKey,const char* _pcVal, TVecChar* _pvecRes );

	/* SELECT <_pcFlt> FROM <m_strTableName> WHERE <_pcKey> ='< _pcVal>' */
	bool Select(const char* _pcFlt, const char* _pcKey,const char* _pcVal, TTable* _pTable );
    
    /* DELETE FROM <m_strTableName> WHERE <_pcKey> ='< _pcVal>' */
    void Delete(const char* _pcKey, const char* _pcVal);

	/* INSERT INTO <m_strTableName> VALUE (<_cszVal>) */
	void Insert( const char* _cszVal); 

	/* INSERT INTO <m_strTableName> (_vecCols[0] ... _vecCols[n])  VALUES (_vecValues[0] ... _vecValues[n]) */
	bool Insert(const TVecMyStr& _vecCols, const TVecMyStr& _vecValues ); 

	/* INSERT INTO <m_strTableName> (strCols)  VALUES (strVal) */
	void Insert( const CMyStr* strCol, const CMyStr* strVal );

	/* UPDATE <m_strTableName> SET  <_cszCol>=<_cszNewVal> WHERE <_cszKey> =< _cszVal>;*/
	void Update( const char* _cszCol, const char* _cszNewVal, const char* _cszKey, const char* _cszVal ); 

	/* SELECT <_pcFlt> FROM <m_strTableName> WHERE <_pcKey> */
	bool Select(const char* _pcFlt, const char* _pcKey, TTable* _pTable );

	int LastInsertId() { return m_sqlBase.LastInsertId();  }


	static void ar2blob(const TVecByte& _cvecIn, CMyStr* _pRes);

private:

	void CopyToTable(TTable* _pTable, MYSQL_RES* pRes);

private:

	CSqlBase	m_sqlBase;

	CMyStr		m_strTableName;
};

#endif
