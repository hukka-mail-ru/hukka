#ifndef SQLGAMETABLE_H
#define SQLGAMETABLE_H

#include "sqltable.h"
#include "stdint.h"

/**
	@author Aleksey P. Karelin <karelin@wapportal.ru>
*/

typedef std::vector< uint32_t > TVecUINT;

class SqlGameTable : protected SqlTable
{
public:
	SqlGameTable( char* _cTableName, char* _cTableStruct );

	~SqlGameTable();

	bool getState( uint32_t _nTableID, uint8_t &_nState );

	bool getDrawState( uint32_t _nTableID, uint8_t &_nState );

	bool getIDPlayer0( uint32_t _nTableID, uint32_t &_nIDCreator );

	bool getIDPlayer1( uint32_t _nTableID, uint32_t &_nIDPlayer2 );

	void delIDPlayer1( uint32_t _nTableID);

	bool getPassword( uint32_t _nTableID, TVecChar *_strPassword );

	bool getMinRating( uint32_t _nTableID, uint32_t &_nMinRating );

	bool getMaxRating( uint32_t _nTableID, uint32_t &_nMaxRating );

	bool setState( uint32_t _nTableID, uint8_t _nState );

	bool setDrawState( uint32_t _nTableID, uint8_t _nState );

    bool getTime2Step( uint32_t _nTableID, uint32_t &_nTime2Step );

    bool getTime2Game( uint32_t _nTableID, uint32_t &_nTime2Game );

    void setIDPlayer1( uint32_t _nTableID, uint32_t _nIDPlayer2 );

    void updateFieldState( uint32_t _nTableID, const TVecChar *_vecData );

    bool getStepNum( uint32_t _nTableID, uint32_t &_nStepNum );

    void setStepNum( uint32_t _nTableID, uint32_t _nStepNum );

    bool selectFromStatus( uint8_t _nStatus, TVecUINT* vecRes );

    void setCurPlayer( uint32_t _nTableID, uint32_t _nPlayerID );

    void setPlayerGameTime( uint32_t _nTableID, uint8_t _nPlayerNum, uint32_t _nValue );

    void setPlayerStepTime( uint32_t _nTableID, uint32_t _nValue );

    bool getFieldState( uint32_t _nTableID, TVecChar* _vecField );

    bool getXPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer );

    void setXPlayer( uint32_t _nTableID, uint32_t _nXPlayer );

    bool getCurPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer );

    bool getPlayerStepTime( uint32_t _nTableID, uint32_t &_nStepTime );

    bool getPlayerGameTime( uint32_t _nTableID, uint32_t &_nGameTime );

    bool getPlayerGameTime( uint32_t _nTableID, uint8_t _nPlayerNum, uint32_t &_nGameTime );

private:

    int vec2i(TVecChar *_pvecData);

};

#endif
