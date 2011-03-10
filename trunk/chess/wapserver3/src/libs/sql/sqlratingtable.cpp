#include "sqlratingtable.h"

#include "mystr.h"
#include "sstream"
#include <stdlib.h>

#define MYDEBUG

CSqlRatingTable::CSqlRatingTable( const char* _strTableName, uint32_t _nInitialRating )
: SqlTable( _strTableName,
	"PlayerID INT UNSIGNED NOT NULL,\
Rating INT UNSIGNED NOT NULL,\
PRIMARY KEY ( PlayerID )" )
{
	m_nInitialRating = _nInitialRating;
}

uint32_t CSqlRatingTable::getRating( uint32_t _nPlayerID )
{
#ifdef MYDEBUG
    std::cout << "CSqlRatingTable::getRating()" << std::endl;
#endif
	TVecChar vecData;

	if( !SelectToStr( "Rating", "PlayerID", CMyStr( _nPlayerID ).c_str(), &vecData ) )
	{
#ifdef MYDEBUG
        std::cout << "CSqlRatingTable::getRating() !SelectToStr " << std::endl;
#endif
        setRating( _nPlayerID, m_nInitialRating );
		SelectToStr( "Rating", "PlayerID", CMyStr( _nPlayerID ).c_str(), &vecData );
	}
#ifdef MYDEBUG
    std::cout << "CSqlRatingTable::getRating() vecData size: " << vecData.size() << std::endl;
#endif

    return vec2i( &vecData );
}

void CSqlRatingTable::setRating( uint32_t _nPlayerID, uint32_t _nRating )
{
//#ifdef MYDEBUG
    std::cout << "CSqlRatingTable::setRating(). _nPlayerID " <<  _nPlayerID  << " _nRating " << _nRating << std::endl;
//#endif

    TVecMyStr strvecCol;
    TVecMyStr strvecVal;

    CMyStr strPlayerID = CMyStr("PlayerID");
    CMyStr strRating = CMyStr("Rating");

    strvecCol.push_back(&strPlayerID);
    strvecCol.push_back(&strRating);

    CMyStr strnPlayerID = CMyStr(_nPlayerID);
    CMyStr strnRating = CMyStr(_nRating);

    strvecVal.push_back(&strnPlayerID);
    strvecVal.push_back(&strnRating);


    TVecChar vecTmp;

	if ( !SelectToStr( "Rating", "PlayerID", CMyStr( _nPlayerID ).c_str(), &vecTmp ) )
    {
        SqlTable::Insert( strvecCol, strvecVal );
    }
	else
    {
        SqlTable::Update( "Rating", CMyStr( _nRating ).c_str(),
				   "PlayerID", CMyStr( _nPlayerID ).c_str() );
    }
}

CSqlRatingTable::~CSqlRatingTable()
{
}

int CSqlRatingTable::vec2i(TVecChar *_pvecData)
{
    std::string str(_pvecData->begin(), _pvecData->end());

    return atoi(str.c_str());
}


