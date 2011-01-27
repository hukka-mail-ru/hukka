/******************************************************************************
*
*       (C) Copyright 2007, WapPortal.RU, Ltd. All Rights Reserved.
*
*       THIS SOURCE CODE IS CONFIDENTIAL AND PROPRIETARY AND MAY NOT
*       BE USED OR DISTRIBUTED WITHOUT THE WRITTEN PERMISSION OF
*       WapPortal.RU Ltd.
*
* Filename:     $Source: /home/cvs/wapserver3/testsql/testsql.cpp,v $
* Author:       $Author: boon $
* Locker:       $Locker:  $
* State:        $State: Exp $
* Revision:     $Revision: 1.10 $
*
* History:      Use the CVS command log to display revision history
*               information.
*
* Description:
*
* Notes:
*
******************************************************************************/

#include "testsql.h"

#include "../libs/sql/sqltable.h"
#include <stdio.h>

SqlTable tstBlob(0, 0, "TestDB", "tstBlob");
SqlTable tstSelect(0, 0, "TestDB", "tstSelect");

int main(int argc, char *argv[])
{

	char resGoodBeg[] = { (char)0xCB, (char)0xA0 };
	int nCount = sizeof(resGoodBeg) / sizeof(resGoodBeg[0]);

	int nRes = TestSelect(resGoodBeg, nCount, "1");

	nRes += TestInsert();

	char resGood[] = { (char)0xa, (char)0xb };
	nCount = sizeof(resGood) / sizeof(resGood[0]);

	nRes += TestSelect(resGood, nCount, "3");

	nRes += TestUpdate();

	char resGoodUpdate[] = { (char)0x0, (char)0x0 };
	nCount = sizeof(resGoodUpdate) / sizeof(resGoodUpdate[0]);

	nRes += TestSelect(resGoodUpdate, nCount, "3");

	nRes += TestMultiSelect();

	return nRes;
}

int TestSelect(const char* resGood, int nCount, const char* _cszKey)
{
	TVecChar res;


	tstBlob.SelectToStr("arr", "nbr", _cszKey, &res);

    bool isGood = true;

	if ( res.size() !=  nCount )
	{
		printf("Error : incorrect size\r\n");
		isGood = false;
		return 1;
	}

	printf("result: "); 
	for( int i = 0; i < nCount; ++i )
	{
		if (res.at(i) != resGood[i] )
		{
			printf("...\r\nBad SELECT result : res.at(%i) = 0x%X resGood[%i] = 0x%X \r\n", 
				i, (int)res.at(i), i, (int)resGood[i]);
			isGood = false;
			return 1;
		}
		else
			printf("res.at(%i) = 0x%X ", i, (unsigned int)res.at(i)&0xFF); 

	}

	printf("\r\ntest ok\r\n");

	return 0;
}

int TestInsert()
{
	//INSERT INTO tstBlob (nbr, name, arr) VALUES (3, 'test3', 0x0a0b);

	CMyStr arCols[] = {CMyStr("nbr") , CMyStr("name"),  CMyStr("arr") };
	CMyStr arVals[] = {CMyStr("3") ,   CMyStr("'test3'"), CMyStr("0x0a0b") };

	TVecMyStr vecCols;
	TVecMyStr vecVals;
	
	vecCols.push_back(arCols);
	vecCols.push_back(arCols+1);
	vecCols.push_back(arCols+2);

	vecVals.push_back(arVals);
	vecVals.push_back(arVals+1);
	vecVals.push_back(arVals+2);

	tstBlob.Insert(vecCols, vecVals);

	return 0;
}


int TestUpdate()
{
	char ar[] = { 0x0, 0x0 };

	TVecByte vecByte(2);

	std::copy(ar, ar + sizeof(ar) / sizeof(ar[0]), vecByte.begin());

	CMyStr strBlob;

	SqlTable::ar2blob(vecByte, &strBlob);

	tstBlob.Update("arr", strBlob.c_str(),"nbr","3");

	return 0;
}


int TestMultiSelect()
{
	TTable tbl;

	/* load all table : SELECT * FROM tstSelect WHERE 1 = 1 */
	tstSelect.Select("*","1","1", &tbl);

	printf("Test MultiSelect\n");
	for(int i = 0; i < tbl.size(); ++i )
	{

		for(int j = 0; j < tbl.at(i).size(); ++j)
		{
			const char* str = tbl[i][j].c_str();
			if (tbl[i][j].size())
				printf("%s	", str);	
			else
				printf("NULL	");	
		}

		printf("\n");

	}

	return 0;

}