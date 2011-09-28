#include "sqlgametable.h"

#include <sstream>
#include <stdint.h>
#include <stdlib.h>

//#include <syslog.h>

SqlGameTable::SqlGameTable( char* _cTableName, char* _cTableStruct )
	: SqlTable( _cTableName, _cTableStruct)
{
}

bool SqlGameTable::getIDPlayer0( uint32_t _nTableID, uint32_t &_nIDCreator )
{
	TVecChar vecData;

	if( !SelectToStr( "IDPlayer0", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

    _nIDCreator = vec2i( &vecData );

	return true;
}

bool SqlGameTable::getIDPlayer1( uint32_t _nTableID, uint32_t &_nIDPlayer2 )
{
	TVecChar vecData;

	if( !SelectToStr( "IDPlayer1", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

    _nIDPlayer2 = vec2i( &vecData );

	return true;
}

bool SqlGameTable::getMaxRating( uint32_t _nTableID, uint32_t &_nMaxRating )
{
	TVecChar vecData;

	if( !SelectToStr( "MaxRating", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	if ( !vecData.empty() )
        _nMaxRating = vec2i( &vecData );
	else
		_nMaxRating = 0;

	return true;
}

bool SqlGameTable::getMinRating( uint32_t _nTableID, uint32_t &_nMinRating )
{
	TVecChar vecData;

	if( !SelectToStr( "MinRating", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;
	if ( !vecData.empty() )
        _nMinRating = vec2i( &vecData );
	else
		_nMinRating = 0;

	return true;
}

bool SqlGameTable::getPassword( uint32_t _nTableID, TVecChar *_strPassword )
{
	if( !SelectToStr( "Password", "TableID", CMyStr( _nTableID ).c_str(), _strPassword ) )
		return false;

	return true;
}

bool SqlGameTable::getState( uint32_t _nTableID, uint8_t &_nState )
{
	TVecChar vecData;

	if( !SelectToStr( "State", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
	{
		return false;
	}

    _nState = vec2i( &vecData );

	return true;
}

bool SqlGameTable::getDrawState( uint32_t _nTableID, uint8_t &_nDrawState )
{
	TVecChar vecData;

	if( !SelectToStr( "DrawState", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
	{
		return false;
	}

    _nDrawState = vec2i( &vecData );

	return true;
}


bool SqlGameTable::setState( uint32_t _nTableID, uint8_t _nState )
{
	SqlTable::Update( "State", CMyStr( _nState ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
	return true;
}


bool SqlGameTable::setDrawState( uint32_t _nTableID, uint8_t _nDrawState )
{
	SqlTable::Update( "DrawState", CMyStr( _nDrawState ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
	return true;
}


SqlGameTable::~SqlGameTable()
{
}

/*!
    \fn SqlGameTable::getTime2Step( uint32_t _nTableID, uint32_t &_nTime2Step )
 */
bool SqlGameTable::getTime2Step( uint32_t _nTableID, uint32_t &_nTime2Step )
{
	TVecChar vecData;

	if( !SelectToStr( "Time2Step", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

    _nTime2Step = vec2i( &vecData );

	return true;
}


/*!
    \fn SqlGameTable::getTime2Game( uint32_t _nTableID, uint32_t &_nTime2Game )
 */
bool SqlGameTable::getTime2Game( uint32_t _nTableID, uint32_t &_nTime2Game )
{
	TVecChar vecData;

	if( !SelectToStr( "Time2Game", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

    _nTime2Game = vec2i( &vecData );

	return true;
}

/*!
    \fn SqlGameTable::setIDPlayer1( uint32_t _nTableID, uint32_t _nIDPlayer2 )
 */
void SqlGameTable::setIDPlayer1( uint32_t _nTableID, uint32_t _nIDPlayer2 )
{
	if (_nIDPlayer2 > 0)
		SqlTable::Update( "IDPlayer1", CMyStr( _nIDPlayer2 ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
	else
		SqlTable::Update( "IDPlayer1", "NULL", "TableID", CMyStr( _nTableID ).c_str() );
}

void SqlGameTable::delIDPlayer1( uint32_t _nTableID)
{
	SqlTable::Update( "IDPlayer1", "NULL", "TableID", CMyStr( _nTableID ).c_str() );
}

void SqlGameTable::setXPlayer( uint32_t _nTableID, uint32_t _nXPlayer )
{
	if (_nXPlayer > 0)
		SqlTable::Update( "XPlayer", CMyStr( _nXPlayer ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
	else
		SqlTable::Update( "XPlayer", "NULL", "TableID", CMyStr( _nTableID ).c_str() );
}


/*!
    \fn SqlGameTable::updateFieldState( uint32_t _nTableID, const TVecChar *_vecData )
 */
void SqlGameTable::updateFieldState( uint32_t _nTableID, const TVecChar *_vecData )
{
	CMyStr strBLOB;

	TVecChar vecField;

	for ( TVecChar::const_iterator i = _vecData->begin(); i!= _vecData->end(); ++i )
		vecField.push_back( *i );

	ar2blob( vecField, &strBLOB );

	SqlTable::Update( "FieldState", strBLOB.c_str(), "TableID", CMyStr( _nTableID ).c_str() );

#ifdef GMS_DEBUG
        std::cerr << "SqlGameTable::playerStep strBLOB"  <<  strBLOB.c_str() << std::endl << std::endl;
#endif
}


/*!
    \fn SqlGameTable::getStepNum( uint32_t _nTableID, uint32_t &_nStepNum )
 */
bool SqlGameTable::getStepNum( uint32_t _nTableID, uint32_t &_nStepNum )
{
	TVecChar vecData;

	if( !SelectToStr( "StepNum", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

    _nStepNum = vec2i( &vecData );

	return true;
}


/*!
    \fn SqlGameTable::setStepNum( uint32_t _nTableID, uint32_t _nStepNum )
 */
void SqlGameTable::setStepNum( uint32_t _nTableID, uint32_t _nStepNum )
{
	SqlTable::Update( "StepNum", CMyStr( _nStepNum ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}


/*!
    \fn SqlGameTable::selectFromStatus( uint8_t _nStatus, TVecUINT* vecRes )
 */
bool SqlGameTable::selectFromStatus( uint8_t _nStatus, TVecUINT* vecRes )
{
	TTable tbl;

	if ( ! SqlTable::Select("TableID","State", CMyStr( _nStatus ).c_str(), &tbl) )
		return false;

	vecRes->resize(tbl.size());

	for(TTable::const_iterator it = tbl.begin(); it != tbl.end(); ++it)
	{
		vecRes->at( it - tbl.begin() ) = atoi(it->at(0).c_str());
	}

	return true;
}

void SqlGameTable::setCurPlayer( uint32_t _nTableID, uint32_t _nValue )
{
	SqlTable::Update( "CurPlayer", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}

void SqlGameTable::setPlayerGameTime( uint32_t _nTableID, uint8_t _nPlayerNum, uint32_t _nValue )
{
	switch ( _nPlayerNum )
	{
		case 0:
			SqlTable::Update( "Player0GameTime", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
			break;
		case 1:
			SqlTable::Update( "Player1GameTime", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
			break;
	}
}

/*!
    \fn SqlGameTable::setPlayerStepTime( uint32_t _nTableID, uint32_t _nValue )
 */
void SqlGameTable::setPlayerStepTime( uint32_t _nTableID, uint32_t _nValue )
{
	SqlTable::Update( "PlayerStepTime", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}

/*!
    \fn SqlGameTable::getFieldState( uint32_t _nTableID, TVecChar* _vecField )
 */
bool SqlGameTable::getFieldState( uint32_t _nTableID, TVecChar* _vecField )
{
	if( !SelectToStr( "FieldState", "TableID", CMyStr( _nTableID ).c_str(), _vecField ) )
		return false;

	return true;
}

bool SqlGameTable::getXPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer )
{
	TVecChar vecData;

	if( !SelectToStr( "XPlayer", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

    _nCurPlayer = vec2i( &vecData );

	return true;
}

/*!
    \fn SqlGameTable::getCurPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer )
 */

bool SqlGameTable::getCurPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer )
{
	TVecChar vecData;

	if( !SelectToStr( "CurPlayer", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

    _nCurPlayer = vec2i( &vecData );

	return true;
}

/*!
    \fn SqlGameTable::getPlayerStepTime( uint32_t _nTableID, uint32_t &_nStepTime )
 */
bool SqlGameTable::getPlayerStepTime( uint32_t _nTableID, uint32_t &_nStepTime )
{
	TVecChar vecData;

	if( !SelectToStr( "PlayerStepTime", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

    _nStepTime = vec2i( &vecData );

	return true;
}


/*!
    \fn SqlGameTable::getPlayerGameTime( uint32_t _nTableID, uint32_t &_nGameTime )
 */
bool SqlGameTable::getPlayerGameTime( uint32_t _nTableID, uint8_t _nPlayerNum, uint32_t &_nGameTime )
{
	TVecChar vecData;
	const char* cszFieldName = 0;

	if (_nPlayerNum == 0)
	{
		cszFieldName =  "Player0GameTime";
	}
	else if (_nPlayerNum == 1)
	{
		cszFieldName =  "Player1GameTime";
	}
	else
	{
		return false;
	}

	if( !SelectToStr( cszFieldName, "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
	{
		return false;
	}

    _nGameTime = vec2i( &vecData );

	return true;
}



