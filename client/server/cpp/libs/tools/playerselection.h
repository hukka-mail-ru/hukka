#ifndef PLAYERSELECTION_H
#define PLAYERSELECTION_H

/**
	@author Aleksey P. Karelin <karelin@wapportal.ru>
*/

#include <stdint.h>

#include "../sql/sqlgametable.h"
#include "../sql/sqlratingtable.h"

class CPlayerSelection
{
public:
	CPlayerSelection( CSqlGameTable *_pGameTable, CSqlRatingTable *_pRatingTable );
	
	~CPlayerSelection();
	
	bool	checkContender( uint32_t _nContenderID, uint32_t _nTableID );
	
private:
	
	CSqlGameTable	*m_pGameTable;
	CSqlRatingTable *m_pRatingTable;

};

#endif
