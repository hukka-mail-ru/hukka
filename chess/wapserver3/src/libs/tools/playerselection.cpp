
#include "playerselection.h"

#include <syslog.h>

CPlayerSelection::CPlayerSelection( SqlGameTable *_pGameTable, CSqlRatingTable *_pRatingTable )
{
	m_pGameTable = _pGameTable;
	m_pRatingTable = _pRatingTable;
}

bool CPlayerSelection::checkContender( uint32_t _nContenderID, uint32_t _nTableID )
{
	/// @todo Помимо проверки на рейтинг реализовать проверку на "Черный список"
	
	uint8_t nState;
	uint32_t nIDPlayer1;
	
	if ( m_pGameTable->getIDPlayer1(_nTableID, nIDPlayer1) )
	{
		return false;
	}
	
	if ( !m_pGameTable->getState( _nTableID, nState ) )
	{
		return false;
	}
	
	if ( nState != 1 )
	{
		return false;
	}
	
	uint32_t nContenderRating = 0, nMinRating = 0, nMaxRating = 0;
	
	nContenderRating = m_pRatingTable->getRating( _nContenderID );

	bool isMinSet = m_pGameTable->getMinRating( _nTableID, nMinRating ); 

	
	bool isMaxSet = m_pGameTable->getMaxRating( _nTableID, nMaxRating );  
		
	if ( ( !isMaxSet || nContenderRating <= nMaxRating ) && ( !isMinSet || nContenderRating >= nMinRating ) )
	{
		return true;
	}
	
	if ( ( nMaxRating == 0 ) && ( nMinRating == 0 ) )
	{
		return true;
	}
	
	return false;
}

CPlayerSelection::~CPlayerSelection()
{
}
