/*
 * SqlAccountTable.cpp
 *
 *  Created on: Sep 28, 2011
 *      Author: ssy
 */

#include "sqlaccounttable.h"
#include <iostream>

using namespace std;

SqlAccountTable::SqlAccountTable(): SqlTable( "tbAccount", "" )
{
    // TODO Auto-generated constructor stub

}

SqlAccountTable::~SqlAccountTable() {
    // TODO Auto-generated destructor stub
}



uint32_t SqlAccountTable::getBalance( uint32_t playerID )
{
    TVecChar v;
    if( !SelectToStr( "Balance", "PlayerID", CMyStr( playerID ).c_str(), &v ) )
    {
        cerr << "SqlAccountTable::getBalance error!" << endl;
        return 0;
    }

    return  vec2i( &v );
}


void SqlAccountTable::addToBalance( uint32_t playerID, uint32_t value )
{
    TVecMyStr parameters;
    CMyStr playerID_str = CMyStr( playerID );
    CMyStr value_str = CMyStr( value );
    parameters.push_back(&playerID_str);
    parameters.push_back(&value_str);

    Call("AddToBalance", parameters);
}
