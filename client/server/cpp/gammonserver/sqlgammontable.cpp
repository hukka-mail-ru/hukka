#include "sqlgammontable.h"

CSqlGammonTable::CSqlGammonTable()
	: CSqlGameTable("tbGammonTableList",
/*	"TableID INT UNSIGNED NOT NULL AUTO_INCREMENT,\
			State TINYINT UNSIGNED NOT NULL,\
			IDPlayer0 INT UNSIGNED NOT NULL,\
			IDPlayer1 INT UNSIGNED NULL,\
			password VARCHAR(50) NULL,\
			Time2Step INT UNSIGNED NOT NULL DEFAULT 30,\
			Time2Game INT UNSIGNED NOT NULL DEFAULT 300,\
			MinRating INT UNSIGNED NULL,\
			MaxRating INT UNSIGNED NULL,\
			StepNum SMALLINT UNSIGNED NULL,\
			CurPlayer INT UNSIGNED NULL,\
			PlayerStepTime INT UNSIGNED NULL,\
			Player0GameTime INT UNSIGNED NULL DEFAULT 0,\
			Player1GameTime INT UNSIGNED NULL DEFAULT 0,\
    		Cube1 TINYINT UNSIGNED NULL,\
			Cube2 TINYINT UNSIGNED NULL,\
			FieldState TINYBLOB NULL,\
			PRIMARY KEY( TableID )"*/
// SZ:
    "TableID INT UNSIGNED NOT NULL AUTO_INCREMENT,\
			State TINYINT UNSIGNED NOT NULL DEFAULT 1,\
			IDPlayer0 INT UNSIGNED NOT NULL,\
			IDPlayer1 INT UNSIGNED NULL,\
			password VARCHAR(50) NULL,\
			Time2Step INT UNSIGNED NOT NULL DEFAULT 30,\
			Time2Game INT UNSIGNED NOT NULL DEFAULT 300,\
			MinRating INT UNSIGNED NULL,\
			MaxRating INT UNSIGNED NULL,\
			StepNum SMALLINT UNSIGNED NULL,\
			XPlayer INT UNSIGNED NULL,\
		        CurPlayer INT UNSIGNED NULL,\
			PlayerStepTime INT UNSIGNED NULL,\
			Player0GameTime INT UNSIGNED NULL DEFAULT 0,\
			Player1GameTime INT UNSIGNED NULL DEFAULT 0,\
			FieldState TINYBLOB NULL,\
			PRIMARY KEY( TableID )")
{
}


CSqlGammonTable::~CSqlGammonTable()
{
}

/*!
    \fn CSqlGammonTable::setStepNum( uint32_t  _nTableID, uint32_t _nValue )
 */
//TODO remove
void CSqlGammonTable::setStepNum( uint32_t  _nTableID, uint32_t _nValue )
{
	CSqlTable::Update( "StepNum", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}


/*!
    \fn CSqlGammonTable::setOpGameTime( uint32_t _nTableID, uint32_t _nValue )
 */
void CSqlGammonTable::setOpGameTime( uint32_t _nTableID, uint32_t _nValue )
{
	CSqlTable::Update( "OpGameTime", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}


/*!
    \fn CSqlGammonTable::setPlayerGameTime( uint32_t _nTableID, uint32_t _nValue )
 */
void CSqlGammonTable::setPlayerGameTime( uint32_t _nTableID, uint8_t _nPlayerNum, uint32_t _nValue )
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
    \fn CSqlGammonTable::setPlayerStepTime( uint32_t _nTableID, uint32_t _nValue )
 */
void CSqlGammonTable::setPlayerStepTime( uint32_t _nTableID, uint32_t _nValue )
{
	CSqlTable::Update( "PlayerStepTime", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}

/*!
    \fn CSqlGammonTable::getFieldState( uint32_t _nTableID, TVecChar* _vecField )
 */
bool CSqlGammonTable::getFieldState( uint32_t _nTableID, TVecChar* _vecField )
{
	if( !SelectToStr( "FieldState", "TableID", CMyStr( _nTableID ).c_str(), _vecField ) )
		return false;

	return true;
}

/*!
    \fn CSqlGammonTable::getCurPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer )
 */

bool CSqlGammonTable::getCurPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer )
{
	TVecChar vecData;

	if( !SelectToStr( "CurPlayer", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nCurPlayer = atoi( &vecData[0] );

	return true;	
}

/*!
    \fn CSqlGammonTable::setCurPlayer1( uint32_t _nTableID, uint8_t _nValue )
 */


void CSqlGammonTable::setCurPlayer( uint32_t _nTableID, uint32_t _nValue )
{
	CSqlTable::Update( "CurPlayer", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}



// SZ:
bool CSqlGammonTable::getXPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer )
{
	TVecChar vecData;

	if( !SelectToStr( "XPlayer", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nCurPlayer = atoi( &vecData[0] );

	return true;	
}

/*!
    \fn CSqlGammonTable::getCube1( uitn32_t _nTableID, uint8_t& _nCube1 )
 */
/* SZ
bool CSqlGammonTable::getCube1( uint32_t _nTableID, uint8_t& _nCube1 )
{
	TVecChar vecData;

	if( !SelectToStr( "Cube1", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nCube1 = atoi( &vecData[0] );

	return true;	
}

*/
/*!
    \fn CSqlGammonTable::getCube2( uint32_t _nTableID, uint8_t& _nCube2 )
 */
/* SZ
bool CSqlGammonTable::getCube2( uint32_t _nTableID, uint8_t& _nCube2 )
{
	TVecChar vecData;

	if( !SelectToStr( "Cube2", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nCube2 = atoi( &vecData[0] );

	return true;	
}
*/

/*!
    \fn CSqlGammonTable::setCube2( uint32_t _nTableID, uint8_t _nValue )
 */
/* SZ:
void CSqlGammonTable::setCube2( uint32_t _nTableID, uint8_t _nValue )
{
	CSqlTable::Update( "Cube2", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}

*/

/*!
    \fn CSqlGammonTable::setCube1( uint32_t _nTableID, uint8_t _nValue )
 */

/* SZ:
void CSqlGammonTable::setCube1( uint32_t _nTableID, uint8_t _nValue )
{
	CSqlTable::Update( "Cube1", CMyStr( _nValue ).c_str(), "TableID", CMyStr( _nTableID ).c_str() );
}
*/


/*!
    \fn CSqlGammonTable::getPlayerStepTime( uint32_t _nTableID, uint32_t &_nStepTime )
 */
bool CSqlGammonTable::getPlayerStepTime( uint32_t _nTableID, uint32_t &_nStepTime )
{
	TVecChar vecData;

	if( !SelectToStr( "PlayerStepTime", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nStepTime = atoi( &vecData[0] );

	return true;	
}


/*!
    \fn CSqlGammonTable::getPlayerGameTime( uint32_t _nTableID, uint32_t &_nGameTime )
 */
bool CSqlGammonTable::getPlayerGameTime( uint32_t _nTableID, uint32_t &_nGameTime )
{
	TVecChar vecData;

	if( !SelectToStr( "PlayerGameTime", "TableID", CMyStr( _nTableID ).c_str(), &vecData ) )
		return false;

	_nGameTime = atoi( &vecData[0] );

	return true;	
}
