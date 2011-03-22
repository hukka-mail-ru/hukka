#include "sqlratingtable.h"

#include "mystr.h"
#include "sstream"
#include <stdlib.h>
#include <defserver.h>
#include <defservice.h>

//#define MYDEBUG

CSqlRatingTable::CSqlRatingTable( const char* _strTableName, uint32_t _nInitialRating )
: SqlTable( _strTableName,
	"PlayerID INT UNSIGNED NOT NULL,\
Rating INT UNSIGNED NOT NULL,\
PRIMARY KEY ( PlayerID )" )
{
	m_nInitialRating = _nInitialRating;
}

// can return RATING_NOT_AVAILABLE
uint32_t CSqlRatingTable::getRating( uint32_t _nPlayerID )
{
	TVecChar vecData;
	if( !SelectToStr( "Rating", "PlayerID", CMyStr( _nPlayerID ).c_str(), &vecData ) )
	{
        setRating( _nPlayerID, m_nInitialRating );
	}

	uint32_t  res = SelectToStr( "Rating", "Available > 0 AND PlayerID", CMyStr( _nPlayerID ).c_str(), &vecData ) ?
		      vec2i( &vecData ) : RATING_NOT_AVAILABLE;

    return res;
}

uint32_t CSqlRatingTable::getLastGameResult( uint32_t _nPlayerID )
{
    uint32_t res = P_NO_RES;
    TVecChar vecData;
    if( SelectToStr( "LastGameResult", "PlayerID", CMyStr( _nPlayerID ).c_str(), &vecData ) )
    {
        res = vec2i( &vecData );
    }

    return res;
}

// can't return RATING_NOT_AVAILABLE
uint32_t CSqlRatingTable::getRatingEvenUnavailable( uint32_t _nPlayerID )
{
    TVecChar vecData;
    if( !SelectToStr( "Rating", "PlayerID", CMyStr( _nPlayerID ).c_str(), &vecData ) )
    {
        setRating( _nPlayerID, m_nInitialRating );
    }

    SelectToStr( "Rating", "PlayerID", CMyStr( _nPlayerID ).c_str(), &vecData );

    return vec2i( &vecData );
}

void CSqlRatingTable::setRating( uint32_t _nPlayerID, uint32_t _nRating )
{
//    std::cout << "CSqlRatingTable::setRating(). _nPlayerID " <<  _nPlayerID  << " _nRating " << _nRating << std::endl;

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

void CSqlRatingTable::setLastGameResult( uint32_t playerID, int result )
{
    TVecMyStr strvecCol;
    TVecMyStr strvecVal;

    CMyStr strPlayerID = CMyStr("PlayerID");
    CMyStr strResult = CMyStr("LastGameResult");

    strvecCol.push_back(&strPlayerID);
    strvecCol.push_back(&strResult);

    CMyStr strnPlayerID = CMyStr(playerID);
    CMyStr strnResult = CMyStr(result);

    strvecVal.push_back(&strnPlayerID);
    strvecVal.push_back(&strnResult);


    TVecChar vecTmp;

    if ( !SelectToStr( "LastGameResult", "PlayerID", CMyStr( playerID ).c_str(), &vecTmp ) )
    {
        SqlTable::Insert( strvecCol, strvecVal );
    }
    else
    {
        SqlTable::Update("LastGameResult", CMyStr( result ).c_str(),
                        "PlayerID", CMyStr( playerID ).c_str() );
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


