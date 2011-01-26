//
// C++ Implementation: tmctest
//
// Description: 
//
//
// Author: leha <karelin@wapportal.ru>, (C) 2009
//
// Copyright: See COPYING file that comes with this distribution
//
//
#include "tmctest.h"

#include <iostream>

#include "tbmcommands.h"

TMCTest::TMCTest()
{
}


TMCTest::~TMCTest()
{
}

void TMCTest::Run()
{
	if ( !Create() )
	{
		std::cerr << "FAILED" << std::endl;
	}
	else
	{
		std::cerr << "PASSED" << std::endl;
	}
		
	if ( !Find() )
	{
		std::cerr << "FAILED" << std::endl;
	}
	else
	{
		std::cerr << "PASSED" << std::endl;
	}
	if ( !GetMyTable() )
	{
		std::cerr << "FAILED" << std::endl;
	}
	else
	{
		std::cerr << "PASSED" << std::endl;
	}
	if ( !RandomOpponent() )
	{
		std::cerr << "FAILED" << std::endl;
	}
	else
	{
		std::cerr << "PASSED" << std::endl;
	}
	
	if ( !CheckParams() )
	{
		std::cerr << "FAILED" << std::endl;
	}
	else
	{
		std::cerr << "PASSED" << std::endl;
	}
	
	if ( !GetTableParams() )
	{
		std::cerr << "FAILED" << std::endl;
	}
	else
	{
		std::cerr << "PASSED" << std::endl;
	}
		
}

bool TMCTest::CheckParams()
{
	std::cerr << "TbmCommands::CheckParams() ";
	
	TbmCommands TbmCommands;
	TVecPrms vecPrmsNV, vecPrmsV;
	
	const int cnLogicId = 1;
	const int cnPlayer1Id = 111;
	
	vecPrmsNV.push_back( std::make_pair<int, int> (3,-10) );
	
	if ( TbmCommands.Create(cnLogicId, cnPlayer1Id, vecPrmsNV) != TbmCommands::NVPAR )
	{
		return false;
	}
	
	vecPrmsV.push_back( std::make_pair<int, int> (3,100) );
	
	if ( TbmCommands.Create(cnLogicId, cnPlayer1Id, vecPrmsV) != TbmCommands::DONE )
	{
		return false;
	}
			
	return true;
	
}

bool TMCTest::Create()
{
	std::cerr << "TbmCommands::Create() && TbmCommands::LastInsertId() ";
	
	TbmCommands TbmCommands;

	TVecPrms vecPrms;

	const int cnLogicId = 1;
	const int cnPlayer1Id = 105;
	const int cnPlayer2Id = 104;

	if ( TbmCommands.Create(cnLogicId, cnPlayer1Id, vecPrms) != TbmCommands::DONE )
	{
		return false;
	}

	int nIndex = TbmCommands.LastInsertId();

	if ( TbmCommands.Create(cnLogicId, cnPlayer2Id, vecPrms) != TbmCommands::DONE )
	{
		return false;
	}

	int nIndex1 = TbmCommands.LastInsertId();

	return nIndex == nIndex1 - 1;
	
}

bool TMCTest::Find()
{
	std::cerr << "TbmCommands::Find() ";
/*	
	TbmCommands TbmCommands;

	TVecUINT vecRes;
	TVecPrms vecPrms;

	const int cnLogicId = 1;
	const int cnPlayer1Id = 100;
	const int cnPlayer1Id2 = 110;
	const int nCount = 3;
	

	TbmCommands.Find(cnLogicId, cnPlayer1Id, nCount, NULL, &vecRes);

	int nSize0 = vecRes.size();

	if ( TbmCommands.Create(cnLogicId, cnPlayer1Id2, vecPrms) != TbmCommands::DONE )
	{
		return false;
	}

	TbmCommands.Find(cnLogicId, cnPlayer1Id, nCount,  NULL, &vecRes);

	int nSize1 = vecRes.size();

	return (nSize1 - nSize0) == 1;
*/
    return false;
}


bool TMCTest::GetTableParams()
{
	std::cerr << "TbmCommands::GetTableParams() ";
	
	TbmCommands TbmCommands;	
	TVecUINT vecParamIDs;
	TVecPrms vecParams;
	
	const int cnLogicId = 1;
	int nTableID = 1;
	
	vecParamIDs.push_back(3);
	vecParamIDs.push_back(7);
	
	if ( TbmCommands.GetTableParams( cnLogicId, nTableID, vecParamIDs, &vecParams ) )
		return false;
	
	vecParamIDs.clear();
	vecParamIDs.push_back(3);
	vecParamIDs.push_back(4);
	if ( !TbmCommands.GetTableParams( cnLogicId, nTableID, vecParamIDs, &vecParams ) )
		return false;			
	
	return true;

}

bool TMCTest::GetMyTable()
{
	std::cerr << "TbmCommands::GetMyTable() ";
	
	TbmCommands TbmCommands;

	const int cnLogicId = 1;
	const int cnPlayer1Id = 107;

	TVecPrms vecPrms;

	TbmCommands.Create(cnLogicId, cnPlayer1Id, vecPrms);

	int nMyIndex = TbmCommands.LastInsertId();

	TVecUINT vecRes;

	if ( TbmCommands.GetMyTable(cnLogicId, cnPlayer1Id, &vecRes ) )
	{
		return vecRes.at(0) == nMyIndex;
	}

	return false;
}

bool TMCTest::RandomOpponent()
{
	std::cerr << "TbmCommands::RandomOpponent() ";
/*
	TbmCommands TbmCommands;

	const int cnLogicId = 1;
	const int cnPlayer1Id = 105;
	const int cnPlayer1Id1 = 108;
    
	bool bNew, bRes;

	int nRes = TbmCommands.RandomOpponent( cnLogicId, cnPlayer1Id, &bNew );

	if ( nRes != 0 )
		return false;

	nRes = TbmCommands.RandomOpponent( cnLogicId, cnPlayer1Id1, &bNew );

	return ( nRes != 0 ) ;
*/
    return false;
}
