#ifndef SQLRATINGTABLE_H
#define SQLRATINGTABLE_H

#include "sqltable.h"

/**
	@author WapPortal.RU <office@wapportal.ru>
*/
class CSqlRatingTable : private SqlTable
{
public:
	CSqlRatingTable(const char* _strTableName, uint32_t _nInitialRating );

	~CSqlRatingTable();

	uint32_t	getRating( uint32_t _nPlayerID );	             // can return RATING_NOT_AVAILABLE
    uint32_t    getRatingEvenUnavailable( uint32_t _nPlayerID ); // can't return RATING_NOT_AVAILABLE
    uint32_t    getLastGameResult( uint32_t _nPlayerID );

    void		setRating( uint32_t _nPlayerID, uint32_t _nRating );
    void        setLastGameResult( uint32_t playerID, int result );

private:

	uint32_t		m_nInitialRating;

};

#endif
