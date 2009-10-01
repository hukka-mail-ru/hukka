#include "sqlgametable.h"

#include <sstream>

//#include <syslog.h>

CSqlGameTable::CSqlGameTable( char* _cTableName, char* _cTableStruct )
	: CSqlTable( _cTableName, _cTableStruct)
{
}

bool CSqlGameTable::getIDPlayer0( uint32_t _nTableID, uint32_t &_nIDCreator )
{
	TVecChar vecData;

	if( !SelectToStr( "IDPlayer0", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nIDCreator = atoi( &vecData[0] );
	
	return true;
}

bool CSqlGameTable::getIDPlayer1( uint32_t _nTableID, uint32_t &_nIDPlayer2 )
{
	TVecChar vecData;

	if( !SelectToStr( "IDPlayer1", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nIDPlayer2 = atoi( &vecData[0] );

	return true;	
}

bool CSqlGameTable::getMaxRating( uint32_t _nTableID, uint32_t &_nMaxRating )
{
	TVecChar vecData;

	if( !SelectToStr( "MaxRating", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;
	
	if ( !vecData.empty() )
		_nMaxRating = atoi( &vecData[0] );
	else 
		_nMaxRating = 0;

	return true;
}

bool CSqlGameTable::getMinRating( uint32_t _nTableID, uint32_t &_nMinRating )
{
	TVecChar vecData;

	if( !SelectToStr( "MinRating", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;
	if ( !vecData.empty() )
		_nMinRating = atoi( &vecData[0] );
	else
		_nMinRating = 0;

	return true;
}

bool CSqlGameTable::getPassword( uint32_t _nTableID, TVecChar *_strPassword )
{
	if( !SelectToStr( "Password", "TableID", CMyStr( _nTableID ).c_str(), _strPassword ) )
		return false;
	
	return true;
}

bool CSqlGameTable::getState( uint32_t _nTableID, uint8_t &_nState )
{
	TVecChar vecData;
	
	//syslog( LOG_INFO | LOG_LOCAL0, "CSqlGameTable::getState( %d )", _nTableID );
	
	if( !SelectToStr( "State", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
	{
		//syslog( LOG_INFO | LOG_LOCAL0, "CSqlGameTable::getState : return FALSE" );
		return false;
	}

	_nState = atoi( &vecData[0] );
	
	//syslog( LOG_INFO | LOG_LOCAL0, "CSqlGameTable::getState _nState = %d", _nState );

	return true;
} 

bool CSqlGameTable::setState( uint32_t _nTableID, uint8_t _nState )
{	
	CSqlTable::Update( "State", CMyStr( _nState ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );	
}

CSqlGameTable::~CSqlGameTable()
{
}




/*!
    \fn CSqlGameTable::getTime2Step( uint32_t _nTableID, uint32_t &_nTime2Step )
 */
bool CSqlGameTable::getTime2Step( uint32_t _nTableID, uint32_t &_nTime2Step )
{
	TVecChar vecData;

	if( !SelectToStr( "Time2Step", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nTime2Step = atoi( &vecData[0] );

	return true;
}


/*!
    \fn CSqlGameTable::getTime2Game( uint32_t _nTableID, uint32_t &_nTime2Game )
 */
bool CSqlGameTable::getTime2Game( uint32_t _nTableID, uint32_t &_nTime2Game )
{
	TVecChar vecData;

	if( !SelectToStr( "Time2Game", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nTime2Game = atoi( &vecData[0] );

	return true;
}

/*!
    \fn CSqlGameTable::setIDPlayer1( uint32_t _nTableID, uint32_t _nIDPlayer2 )
 */
void CSqlGameTable::setIDPlayer1( uint32_t _nTableID, uint32_t _nIDPlayer2 )
{
	if (_nIDPlayer2 > 0)
		CSqlTable::Update( "IDPlayer1", CMyStr( _nIDPlayer2 ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
	else
		CSqlTable::Update( "IDPlayer1", "NULL", "TableID", CMyStr( _nTableID ).c_str() );
}

void CSqlGameTable::delIDPlayer1( uint32_t _nTableID)
{
	CSqlTable::Update( "IDPlayer1", "NULL", "TableID", CMyStr( _nTableID ).c_str() );
}

void CSqlGameTable::setXPlayer( uint32_t _nTableID, uint32_t _nXPlayer )
{
	if (_nXPlayer > 0)
		CSqlTable::Update( "XPlayer", CMyStr( _nXPlayer ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
	else
		CSqlTable::Update( "XPlayer", "NULL", "TableID", CMyStr( _nTableID ).c_str() );
}


/*!
    \fn CSqlGameTable::updateFieldState( uint32_t _nTableID, const TVecChar *_vecData )
 */
void CSqlGameTable::updateFieldState( uint32_t _nTableID, const TVecChar *_vecData )
{
	CMyStr strBLOB;
	
	TVecChar vecField;
	
	for ( TVecChar::const_iterator i = _vecData->begin(); i!= _vecData->end(); ++i )
		vecField.push_back( *i );
	
	ar2blob( vecField, &strBLOB );
	
	CSqlTable::Update( "FieldState", strBLOB.c_str(), "TableID", CMyStr( _nTableID ).c_str() );    

#ifdef GMS_DEBUG
        std::cerr << "CSqlGameTable::playerStep strBLOB"  <<  strBLOB.c_str() << std::endl << std::endl;
#endif
}


/*!
    \fn CSqlGameTable::getStepNum( uint32_t _nTableID, uint32_t &_nStepNum )
 */
bool CSqlGameTable::getStepNum( uint32_t _nTableID, uint32_t &_nStepNum )
{
	TVecChar vecData;

	if( !SelectToStr( "StepNum", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nStepNum = atoi( &vecData[0] );

	return true;	
}


/*!
    \fn CSqlGameTable::setStepNum( uint32_t _nTableID, uint32_t _nStepNum )
 */
void CSqlGameTable::setStepNum( uint32_t _nTableID, uint32_t _nStepNum )
{
	CSqlTable::Update( "StepNum", CMyStr( _nStepNum ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}


/*!
    \fn CSqlGameTable::selectFromStatus( uint8_t _nStatus, TVecUINT* vecRes )
 */
bool CSqlGameTable::selectFromStatus( uint8_t _nStatus, TVecUINT* vecRes )
{
	TTable tbl;
    
	if ( ! CSqlTable::Select("TableID","State", CMyStr( _nStatus ).c_str(), &tbl) )
		return false;

	vecRes->resize(tbl.size());

	for(TTable::const_iterator it = tbl.begin(); it != tbl.end(); ++it)
	{
		vecRes->at( it - tbl.begin() ) = atoi(it->at(0).c_str());
	}
	
	return true;
}

void CSqlGameTable::setCurPlayer( uint32_t _nTableID, uint32_t _nValue )
{
	CSqlTable::Update( "CurPlayer", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}

void CSqlGameTable::setPlayerGameTime( uint32_t _nTableID, uint8_t _nPlayerNum, uint32_t _nValue )
{
	switch ( _nPlayerNum )
	{
		case 0:
			CSqlTable::Update( "Player0GameTime", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
			break;
		case 1:
			CSqlTable::Update( "Player1GameTime", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
			break;
	}
}

/*!
    \fn CSqlGameTable::setPlayerStepTime( uint32_t _nTableID, uint32_t _nValue )
 */
void CSqlGameTable::setPlayerStepTime( uint32_t _nTableID, uint32_t _nValue )
{
	CSqlTable::Update( "PlayerStepTime", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}

/*!
    \fn CSqlGameTable::getFieldState( uint32_t _nTableID, TVecChar* _vecField )
 */
bool CSqlGameTable::getFieldState( uint32_t _nTableID, TVecChar* _vecField )
{
	if( !SelectToStr( "FieldState", "TableID", CMyStr( _nTableID ).c_str(), _vecField ) )
		return false;

	return true;
}

bool CSqlGameTable::getXPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer )
{
	TVecChar vecData;

	if( !SelectToStr( "XPlayer", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nCurPlayer = atoi( &vecData[0] );

	return true;	
}

/*!
    \fn CSqlGameTable::getCurPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer )
 */

bool CSqlGameTable::getCurPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer )
{
	TVecChar vecData;

	if( !SelectToStr( "CurPlayer", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nCurPlayer = atoi( &vecData[0] );

	return true;	
}

/*!
    \fn CSqlGameTable::getPlayerStepTime( uint32_t _nTableID, uint32_t &_nStepTime )
 */
bool CSqlGameTable::getPlayerStepTime( uint32_t _nTableID, uint32_t &_nStepTime )
{
	TVecChar vecData;

	if( !SelectToStr( "PlayerStepTime", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nStepTime = atoi( &vecData[0] );

	return true;	
}


/*!
    \fn CSqlGameTable::getPlayerGameTime( uint32_t _nTableID, uint32_t &_nGameTime )
 */
bool CSqlGameTable::getPlayerGameTime( uint32_t _nTableID, uint32_t &_nGameTime )
{
	TVecChar vecData;

	if( !SelectToStr( "PlayerGameTime", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nGameTime = atoi( &vecData[0] );

	return true;	
}
