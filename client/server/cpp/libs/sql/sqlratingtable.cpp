#include "sqlratingtable.h"

#include "mystr.h"
#include "sstream"

CSqlRatingTable::CSqlRatingTable( const char* _strTableName, uint32_t _nInitialRating )
: CSqlTable( _strTableName, 
	"PlayerID INT UNSIGNED NOT NULL,\
Rating INT UNSIGNED NOT NULL,\
PRIMARY KEY ( PlayerID )" )
{
	m_nInitialRating = _nInitialRating;
}

uint32_t CSqlRatingTable::getRating( uint32_t _nPlayerID )
{
	TVecChar vecData;

	if( !SelectToStr( "Rating", "PlayerID", CMyStr( _nPlayerID ).c_str(), &vecData ) )
	{
		setRating( _nPlayerID, m_nInitialRating );
		SelectToStr( "Rating", "PlayerID", CMyStr( _nPlayerID ).c_str(), &vecData );
	}

	return atoi( &vecData[0] );		
}

void CSqlRatingTable::setRating( uint32_t _nPlayerID, uint32_t _nRating )
{
	CMyStr strValue( "'"+CMyStr( _nPlayerID )+"' , '"+CMyStr( _nRating )+"'" );
	
	TVecChar vecTmp;
	
	if ( !SelectToStr( "Rating", "PlayerID", CMyStr( _nPlayerID ).c_str(), &vecTmp ) )
		CSqlTable::Insert( strValue.c_str() );
	else
		CSqlTable::Update( "Rating", CMyStr( _nRating ).c_str(),
				   "PlayerID", CMyStr( _nPlayerID ).c_str() );	
}

CSqlRatingTable::~CSqlRatingTable()
{
}


