#ifndef GAMMONTABLE_H
#define GAMMONTABLE_H

/**
	@author WapPortal.RU <office@wapportal.ru>
*/

#include "../libs/socket/mysocket.h"
#include "../libs/tools/playerselection.h"
#include "../libs/header/interface.h"
#include "../libs/tools/mythread.h"

#include "gammonlogic.h"
#include "gammonstructs.h"
#include "sqlgammontable.h"

class CGammonTable : public CMyThread
{
public:

    CGammonTable();

    ~CGammonTable();
     
    void playerStep( uint32_t _nPlayerID, SGammonStep _sStep );
    void setSocket( CMySocket* _pSocket );
    void startGame( uint32_t _nTableID );
    void getField( uint32_t _nPlayerID, uint32_t _nTableID );
    void loose( uint32_t _nPlayerID, uint32_t _nTableID );
    bool joinToTable( uint32_t _nPlayerID, uint32_t _nTableID );
    
private:

	CPlayerSelection	*m_pPlayerSelection;
	CSqlRatingTable		*m_pRatingTable;
	CSqlGammonTable		m_DBTable;
	CMySocket		*m_pSocket;
	CGammonLogic		m_Logic;

	void sendMsg( uint32_t _nTO, void *_spCmd, int _nSize );

    /* SZ: remove 
    void initField();
    void throwCubes( uint32_t _nTableID, uint8_t &_nCube1, uint8_t &_nCube2 );
    */

private:

    void endGame( uint32_t _nTableID, IGameLogic::StepRes _Result );
    void setRating( uint32_t _nWinnerID, uint32_t _nLooserID );
    bool isQueue( uint32_t nPlayerID, uint32_t _nTableID );
    int Run();
};

#endif
