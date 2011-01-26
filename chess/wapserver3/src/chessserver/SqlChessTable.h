#ifndef SQLCHESSTABLE_H_
#define SQLCHESSTABLE_H_

#include "../libs/sql/sqlgametable.h"

class SqlChessTable : public SqlGameTable
{
public:
	SqlChessTable();
	virtual ~SqlChessTable();
};

#endif /*SQLCHESSTABLE_H_*/
