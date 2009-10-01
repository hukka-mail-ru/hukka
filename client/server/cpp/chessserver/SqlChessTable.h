#ifndef SQLCHESSTABLE_H_
#define SQLCHESSTABLE_H_

#include "../libs/sql/sqlgametable.h"

class CSqlChessTable : public CSqlGameTable
{
public:
	CSqlChessTable();
	virtual ~CSqlChessTable();
};

#endif /*SQLCHESSTABLE_H_*/
