//
// C++ Interface: tmctest
//
// Description: 
//
//
// Author: leha <karelin@wapportal.ru>, (C) 2009
//
// Copyright: See COPYING file that comes with this distribution
//
//
#ifndef TMCTEST_H
#define TMCTEST_H

/**
	@author leha <karelin@wapportal.ru>
*/
class TMCTest
{
public:
	TMCTest();

	virtual ~TMCTest();
    
    	void Run();
    
private:
	bool Create();
	bool Find();
	bool GetMyTable();
	bool GetTableParams();
	bool RandomOpponent();
	bool CheckParams();
};

#endif
