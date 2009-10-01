#ifndef SQLGAMMONTABLE_H
#define SQLGAMMONTABLE_H

#include "sqlgametable.h"

/**
	@author WapPortal.RU <office@wapportal.ru>
*/
class CSqlGammonTable : public CSqlGameTable
{
public:
    CSqlGammonTable();

    ~CSqlGammonTable();
    
    void setCurPlayer( uint32_t _nTableID, uint32_t _nPlayerID );
    void setStepNum( uint32_t  _nTableID, uint32_t _nValue );
    void setOpGameTime( uint32_t _nTableID, uint32_t _nValue );
    void setPlayerGameTime( uint32_t _nTableID, uint8_t _nPlayerNum, uint32_t _nValue );
    void setPlayerStepTime( uint32_t _nTableID, uint32_t _nValue );

    bool getFieldState( uint32_t _nTableID, TVecChar* _vecField );

    bool getXPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer );
    bool getCurPlayer( uint32_t _nTableID, uint32_t &_nCurPlayer );
    
    /* SZ:
    bool getCube1( uint32_t _nTableID, uint8_t& _nCube1 );
    bool getCube2( uint32_t _nTableID, uint8_t& _nCube2 );
    void setCube2( uint32_t _nTableID, uint8_t _nValue );
    void setCube1( uint32_t _nTableID, uint8_t _nValue );
    */

    bool getPlayerStepTime( uint32_t _nTableID, uint32_t &_nStepTime );
    bool getPlayerGameTime( uint32_t _nTableID, uint32_t &_nGameTime );
};

#endif
