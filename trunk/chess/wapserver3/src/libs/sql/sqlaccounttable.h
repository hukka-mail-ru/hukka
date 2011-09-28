/*
 * SqlAccountTable.h
 *
 *  Created on: Sep 28, 2011
 *      Author: ssy
 */

#ifndef SQLACCOUNTTABLE_H_
#define SQLACCOUNTTABLE_H_

#include "sqltable.h"

class SqlAccountTable: private SqlTable {
public:
    SqlAccountTable();
    virtual ~SqlAccountTable();

    uint32_t  getBalance( uint32_t playerID );
};

#endif /* SQLACCOUNTTABLE_H_ */
