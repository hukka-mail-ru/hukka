#include "gammontable.h"
#include "gammondefs.h"
#include "../libs/header/defservice.h"
#include <stdlib.h>
#include <time.h>
#include <syslog.h>

CGammonTable::CGammonTable()
{
    m_pRatingTable = new CSqlRatingTable( "tbGammonRating", 1000 );
    m_pPlayerSelection = new CPlayerSelection( &m_DBTable, m_pRatingTable );
}


CGammonTable::~CGammonTable()
{
    //this->StopLoop();
    delete m_pPlayerSelection;
    delete m_pRatingTable;
}

/**
    \fn CGammonTable::startGame( uint32_t _nTableID )
 */
void CGammonTable::startGame( uint32_t _nTableID )
{
    syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::startGame( %d )", _nTableID );

#ifdef GMS_DEBUG
    std::cerr << "CGammonTable::startGame  _nTableID = " << _nTableID << std::endl;
#endif
    
    m_DBTable.setState( _nTableID, ST_GAME );
    
    SGammonBigMsg Msg;
    Msg.m_chCmd = ANS_START;
    Msg.m_nTableID = _nTableID;
    
    uint32_t nPlayer1, nPlayer2;
    m_DBTable.getIDPlayer0( _nTableID, nPlayer1 );
    m_DBTable.getIDPlayer1( _nTableID, nPlayer2 );
    
    CGammonLogic GammonLogic;
    
    m_DBTable.setStepNum( _nTableID, 0 );

    m_DBTable.updateFieldState( _nTableID, GammonLogic.GetPosForDB() );
    
    if ( GammonLogic.GetPosForClient()->at( CGammonLogic::STEP ) == 0 )
        m_DBTable.setCurPlayer( _nTableID, nPlayer1 );
    else
        m_DBTable.setCurPlayer( _nTableID, nPlayer2 );
    
    uint32_t nTime2Game, nTime2Step;
    
    time_t nStepTime = time( NULL );
    
    m_DBTable.setPlayerStepTime( _nTableID, nStepTime );

#ifdef GMS_DEBUG
    std::cerr << "CGammonTable::startGame  Msg = [ ";
    for(int i = 0; i < sizeof(Msg); ++i)
          std::cerr << (int)*((char*)(&Msg) + i) << " ";
    std::cerr << " ] " << std::endl;
#endif    
	Msg.m_nPlayerNbr = 0;
    sendMsg( nPlayer1, &Msg, sizeof( Msg ) );
	Msg.m_nPlayerNbr = 1;
    sendMsg( nPlayer2, &Msg, sizeof( Msg ) );
    
    syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::startGame : sending ANS_START to %d and %d", nPlayer1, nPlayer2 );
}



/**
    \fn CGammonTable::playerStep( uint32_t _nPlayerID, SGammonStep _sStep )
 */
void CGammonTable::playerStep( uint32_t _nPlayerID, SGammonStep _sStep )
{
    uint32_t nPlayer1, nPlayer2;
    TVecChar vecStep, vecFieldState; 
    const TVecChar *pField;
    IGameLogic::StepRes Result;
    
    SGammonMsg sCmd;
    sCmd.m_chCmd = ANS_STEP;
    sCmd.m_nTableID = _sStep.m_nTableID;
    
    SGammonBigMsg sOCmd;
    sOCmd.m_chCmd = ANS_OSTEP;
    sOCmd.m_nTableID = _sStep.m_nTableID;
    

    m_DBTable.getIDPlayer0( _sStep.m_nTableID, nPlayer1 );
    m_DBTable.getIDPlayer1( _sStep.m_nTableID, nPlayer2 );

    for ( int i = 0; i < 8; ++i )    /// @todo Fixme
    {
        vecStep.push_back( _sStep.m_arnStep[i] );
        sOCmd.m_arData[i] = _sStep.m_arnStep[i];
    }

    vecStep.push_back( _sStep.m_btSide );
        
    vecFieldState.reserve( 500 );
    
    if ( m_DBTable.getFieldState( _sStep.m_nTableID, &vecFieldState ) )
    {
        syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep: vecFieldState size = %d", vecFieldState.size() );

#ifdef GMS_DEBUG
        std::cerr << "CGammonTable::playerStep: vecFieldState size =" << vecFieldState.size() << std::endl;
        std::cerr << "CGammonTable::playerStep: _sStep.m_nTableID =" << _sStep.m_nTableID << std::endl;
        for(TVecChar::const_iterator it = vecFieldState.begin(); it != vecFieldState.end(); ++it )
            std::cerr << *it;
#endif
    
        m_Logic.SetPos( vecFieldState );
    
        syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep PlayerID : %d TableID : %d ", _nPlayerID, _sStep.m_nTableID );
        syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep PlayerID : vecStep.size() = %d ", vecStep.size() );
        for (int i = 0; i < vecStep.size(); ++i)    
            syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep : vecStep.at(%d) = %d ", i, vecStep.at(i) );
    
        Result = m_Logic.StepAnl( &vecStep );
    }
    else
        Result = IGameLogic::NotValid;

    
    if ( Result == IGameLogic::Valid )
    {
        // SZ
        pField = m_Logic.GetPosForDB();

#ifdef GMS_DEBUG
        std::cerr << "CGammonTable::playerStep - m_DBTable.updateFieldState"  << std::endl;
        for(TVByte::const_iterator it = pField->begin(); it != pField->end(); ++it)
        {
            std::cerr << *it;
        }
        std::cerr << std::endl;
#endif

        m_DBTable.updateFieldState( _sStep.m_nTableID, pField );

        syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep m_DBTable.updateFieldState( _sStep.m_nTableID, pField )");

        syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep Result == IGameLogic::Valid");
        syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep after m_Logic.StepAnl( &vecStep ) == Valid : vecStep.size() = %d ", vecStep.size() );
        for (int i = 0; i < vecStep.size(); ++i)    
            syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep : vecStep.at(%d) = %d ", i, vecStep.at(i) );
        
        int queue = vecStep.at( 2 );
        if ( queue == 0 )
        {
            m_DBTable.setCurPlayer( _sStep.m_nTableID, nPlayer1 );
            syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep m_DBTable.setCurPlayer( _sStep.m_nTableID, nPlayer1 )");
        }
        else
        {
            m_DBTable.setCurPlayer( _sStep.m_nTableID, nPlayer2 );
            syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep m_DBTable.setCurPlayer( _sStep.m_nTableID, nPlayer2 )");
        }
        
// TODO use in release version!
        /*sCmd.m_chData = (char) P_VALID; 
        sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );                
        syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) )");
        
        if ( _nPlayerID != nPlayer1 )
        {
			sOCmd.m_nPlayerNbr = 0;
            sendMsg( nPlayer1, &sOCmd, sizeof( sOCmd ) );
            syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep sendMsg( nPlayer1, &sOCmd, sizeof( sOCmd ) )");
        }
        else
        {
			sOCmd.m_nPlayerNbr = 1;
            sendMsg( nPlayer2, &sOCmd, sizeof( sOCmd ) );
            syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep sendMsg( nPlayer2, &sOCmd, sizeof( sOCmd ) )");
        }
        
        SGammonCubeMsg cubeMsg;
        
        cubeMsg.m_chCmd = ANS_CUBES;
        cubeMsg.m_nTableID = _sStep.m_nTableID;
        cubeMsg.m_chData = queue;
        cubeMsg.m_chCube1 = vecStep.at( 0 );
        cubeMsg.m_chCube2 = vecStep.at( 1 );
        
        sendMsg( nPlayer1, &cubeMsg, sizeof( cubeMsg ) );
        syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep sendMsg( nPlayer1, &cubeMsg, sizeof( cubeMsg ) )");
        sendMsg( nPlayer2, &cubeMsg, sizeof( cubeMsg ) );        
        syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep sendMsg( nPlayer2, &cubeMsg, sizeof( cubeMsg ) )");
        */
        getField( nPlayer1, _sStep.m_nTableID);
        getField( nPlayer2, _sStep.m_nTableID);
    }    
    else if ( Result == IGameLogic::NotValid )
    {
        sCmd.m_chData = (char) P_NOT_VALID; 
// TODO use in release version!
        //sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );                
        getField( _nPlayerID, _sStep.m_nTableID);
        return;
    }
    else
    {
        endGame( _sStep.m_nTableID, Result );
        // add SZ
        return; // ??? 
    } 
    
    uint32_t nStep;   

    m_DBTable.getStepNum( _sStep.m_nTableID, nStep );
    syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep  m_DBTable.getStepNum( _sStep.m_nTableID, nStep ) #1");
    ++nStep;
    m_DBTable.setStepNum( _sStep.m_nTableID, nStep );
    syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep  m_DBTable.getStepNum( _sStep.m_nTableID, nStep ) #2");

    time_t nStepTime = time( NULL );
    
    m_DBTable.setPlayerStepTime( _sStep.m_nTableID, nStepTime );
    syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::playerStep  m_DBTable.setPlayerStepTime( _sStep.m_nTableID, nStepTime ) - end CGammonTable::playerStep");    

}

/**
    \fn CGammonTable::setSocket( CMySocket* _pSocket )
 */
void CGammonTable::setSocket( CMySocket* _pSocket )
{
    m_pSocket = _pSocket;
    //this->StartLoop();
}

/**
    \fn CGammonTable::getField( uint32_t _nPlayerID, uint32_t _nTableID )
 */
void CGammonTable::getField( uint32_t _nPlayerID, uint32_t _nTableID )
{
    syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::getField( %d , %d )", _nPlayerID, _nTableID );
    
    TVecChar vecField;
    const TVecChar *pvecMsgField;
    uint32_t nPlayer0, nCurPlayer;
    
    SGammonBigMsg sCmd;
	m_DBTable.getIDPlayer0( _nTableID, nPlayer0 );

	if (_nPlayerID == nPlayer0)
		sCmd.m_nPlayerNbr = 0;
	else
		sCmd.m_nPlayerNbr = 1;

    sCmd.m_chCmd = ANS_FIELD;
    sCmd.m_nTableID = _nTableID;
    
    //syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::getField( %d , %d ): DB query", _nPlayerID, _nTableID );

#ifdef GMS_DEBUG
    std::cerr << "CGammonTable::getField: vecField size =" << vecField.size() << std::endl;
    std::cerr << "CGammonTable::getField: _nTableID =" << _nTableID << std::endl;
    std::cerr << "CGammonTable::getField: _nPlayerID =" << _nPlayerID << std::endl;
#endif

    if (  m_DBTable.getFieldState( _nTableID, &vecField ) )
    {
    // SZ: ---------------------------
           CGammonLogic GammonLogic;

#ifdef GMS_DEBUG
        for(TVecChar::const_iterator it = vecField.begin(); it != vecField.end(); ++it )
            std::cerr << *it;
#endif

           GammonLogic.SetPos(vecField);

           pvecMsgField = GammonLogic.GetPosForClient();
    // -------------------------------
    
    //syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::getField( %d , %d ): pvecMsgField size = %d", _nPlayerID, _nTableID, pvecMsgField->size() );

        int k = 0;
    
        for ( TVecChar::const_iterator i = pvecMsgField->begin(); i != pvecMsgField->end(); ++i )
        {
            sCmd.m_arData[k] = *i;
            ++k;
        }
    }
    else
        syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::getField( %d , %d ) : ! m_DBTable.getFieldState", _nPlayerID, _nTableID );

            
    sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );

}

/**
    \fn CGammonTable::loose( uint32_t _nPlayerID, uint32_t _nTableID )
  */
  
void CGammonTable::loose( uint32_t _nPlayerID, uint32_t _nTableID )
{
    uint32_t nCurPlayer;
    
    m_DBTable.getCurPlayer( _nTableID, nCurPlayer );
    
    if ( nCurPlayer == _nPlayerID )
    {
        endGame( _nTableID, IGameLogic::Loose );
    }
    else
    {
        endGame( _nTableID, IGameLogic::Win );
    }
}

/*!
    \fn CGammonTable::joinToTable( uint32_t _nPlayerID, uint32_t _nTableID )
 */
    bool CGammonTable::joinToTable( uint32_t _nPlayerID, uint32_t _nTableID )
{
    //syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::joinToTable( %d , %d )", _nPlayerID, _nTableID );
    
    if ( ! m_pPlayerSelection->checkContender( _nPlayerID, _nTableID ) )
        return false;
    
    //syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::joinToTable m_DBTable.setState( %d, %d)", _nTableID, ST_FULL );
    
    m_DBTable.setState( _nTableID, ST_FULL );
    
    //syslog( LOG_INFO | LOG_LOCAL0, "CGammonTable::joinToTable m_DBTable.setIDPlayer1( %d, %d )", _nTableID , _nPlayerID );
    m_DBTable.setIDPlayer1( _nTableID, _nPlayerID );
    
    
    
    startGame( _nTableID );
    
    return true;    
}

void CGammonTable::sendMsg( uint32_t _nTO, void *_spCmd, int _nSize )
{
    TVecChar vecCmd;

#ifdef MYDEBUG
    //std::cout << "GammonServer Wr:"<< (int) _nTO << "-" << (int)(( SGammonMsg* ) _spCmd)->m_chCmd << "-" << (( SGammonMsg* ) _spCmd)->m_nTableID << "-" << (int)(( SGammonMsg* ) _spCmd)->m_chData << std::endl;     
#endif
    syslog( LOG_INFO | LOG_LOCAL0, "Send to %d CMD: %d TableID: %d Param: %d", _nTO, (int)(( SGammonMsg* ) _spCmd)->m_chCmd,(( SGammonMsg* ) _spCmd)->m_nTableID, (int)(( SGammonMsg* ) _spCmd)->m_chData );
    
    vecCmd.assign( (char*)_spCmd, (char*)(_spCmd) + _nSize );
    
    CClientMsg Msg;
    
    Msg.InitMsg( _nTO, vecCmd );
    
    m_pSocket->AddMsg( Msg );
}


/*!
    \fn CGammonTable::initField()
 */
/* SZ: remove
void CGammonTable::initField()
{
    std::vector<int8_t> vecField;
    vecField.reserve( 26 );
    
    vecField.at( 0 ) = -2;
    vecField.at( 5 ) = 5;
    vecField.at( 7 ) = 3;
    vecField.at( 11 ) = -5;
    vecField.at( 12 ) = 5;
    vecField.at( 16 ) = -3;
    vecField.at( 18 ) = -5;
    vecField.at( 23 ) = 2;

}
*/

/*!
    \fn CGammonTable::throwCubes( uint8_t &_nCube1, uint8_t &_nCube2 )
 */
/* SZ:
void CGammonTable::throwCubes( uint32_t _nTableID, uint8_t &_nCube1, uint8_t &_nCube2 )
{
    uint32_t nPlayer1, nPlayer2;
    
    m_DBTable.getIDPlayer0( _nTableID, nPlayer1 );
    m_DBTable.getIDPlayer1( _nTableID, nPlayer2 );
    
    SGammonBigMsg Msg;
    Msg.m_chCmd = ANS_CUBES;
    Msg.m_nTableID = _nTableID;

    srand( time( NULL ) );
    _nCube1 = 1 + rand() % 6;
    _nCube2 = 1 + rand() % 6;
    
    Msg.m_arData[0] = _nCube1;
    Msg.m_arData[1] = _nCube2;
    
    sendMsg( nPlayer1, &Msg, sizeof( Msg ) );
    sendMsg( nPlayer2, &Msg, sizeof( Msg ) );
    
}
*/

/*!
    \fn CGammonTable::endGame( uint32_t _nTableID, IGameLogic::StepRes _Result )
 */
void CGammonTable::endGame( uint32_t _nTableID, IGameLogic::StepRes _Result )
{
    uint32_t nCurPlayer;
    uint32_t nPlayer1, nPlayer2;
    SGammonMsg sCmd;
    sCmd.m_chCmd = ANS_END;
    sCmd.m_nTableID = _nTableID;
    
    m_DBTable.getIDPlayer0( _nTableID, nPlayer1 );
    m_DBTable.getIDPlayer1( _nTableID, nPlayer2 );
    m_DBTable.getCurPlayer( _nTableID, nCurPlayer );
        
    if ( _Result == IGameLogic::Win )
    {
        if ( nCurPlayer == nPlayer1 )
        {
            sCmd.m_chData = (char) P_WIN; 
            sendMsg( nPlayer1, &sCmd, sizeof( sCmd ) );
            
            sCmd.m_chData = (char) P_LOOSE;
            sendMsg( nPlayer2, &sCmd, sizeof( sCmd ) );
                    
            m_DBTable.setState( _nTableID, ST_WIN_P1 );
            setRating( nPlayer1, nPlayer2 );
        }
        else
        {
            sCmd.m_chData = ( char ) P_LOOSE;
            sendMsg( nPlayer1, &sCmd, sizeof( sCmd ) );
                        
            sCmd.m_chData = ( char ) P_WIN;
            sendMsg( nPlayer2, &sCmd, sizeof( sCmd ) );    
                
            m_DBTable.setState( _nTableID, ST_WIN_P2 );
            setRating( nPlayer2, nPlayer1 );
        }
                
    }
    
    if ( _Result == IGameLogic::Loose )
    {
        if ( nCurPlayer == nPlayer1 )
        {
            sCmd.m_chData = ( char ) P_LOOSE;
            sendMsg( nPlayer1, &sCmd, sizeof( sCmd ) );
                
            sCmd.m_chData = ( char ) P_WIN;
            sendMsg( nPlayer2, &sCmd, sizeof( sCmd ) );
            
            m_DBTable.setState( _nTableID, ST_WIN_P2 );
            setRating( nPlayer2, nPlayer1 );
        }
        else
        {
        
            sCmd.m_chData = (char) P_WIN; 
            sendMsg( nPlayer1, &sCmd, sizeof( sCmd ) );
                        
            sCmd.m_chData = (char) P_LOOSE;
            sendMsg( nPlayer2, &sCmd, sizeof( sCmd ) );
            
            m_DBTable.setState( _nTableID, ST_WIN_P1 );
            setRating( nPlayer1, nPlayer2 );
        }        
        
    }    
  
    if ( _Result == IGameLogic::TimeOut )
    {
        if ( nCurPlayer == nPlayer1 )
        {
            sCmd.m_chData = ( char ) P_LOOSE_TIME;
            sendMsg( nPlayer1, &sCmd, sizeof( sCmd ) );
                
            sCmd.m_chData = ( char ) P_WIN;
            sendMsg( nPlayer2, &sCmd, sizeof( sCmd ) );
            
            m_DBTable.setState( _nTableID, ST_WIN_P2 );
            setRating( nPlayer2, nPlayer1 );
        }
        else
        {
        
            sCmd.m_chData = (char) P_WIN; 
            sendMsg( nPlayer1, &sCmd, sizeof( sCmd ) );
                        
            sCmd.m_chData = (char) P_LOOSE_TIME;
            sendMsg( nPlayer2, &sCmd, sizeof( sCmd ) );
            
            m_DBTable.setState( _nTableID, ST_WIN_P1 );
            setRating( nPlayer1, nPlayer2 );
        }        
        
    }    

}


/*!
    \fn CGammonTable::setRating( uint32_t _nWinnerID, uint32_t _nLooserID )
 */
void CGammonTable::setRating( uint32_t _nWinnerID, uint32_t _nLooserID )
{
    uint32_t nWinnerRating = m_pRatingTable->getRating( _nWinnerID );
    uint32_t nLooserRating = m_pRatingTable->getRating( _nLooserID );
    uint32_t nTmp = nWinnerRating;
    
    nWinnerRating += nLooserRating * 0.1;
    nLooserRating -= nLooserRating * 0.1;    
    
    m_pRatingTable->setRating( _nWinnerID, nWinnerRating );
    m_pRatingTable->setRating( _nLooserID, nLooserRating );
}


/*!
    \fn CGammonTable::isQueue( uint32_t nPlayerID, uint32_t _nTableID )
 */
bool CGammonTable::isQueue( uint32_t _nPlayerID, uint32_t _nTableID )
{
    uint32_t nCurPlayerID;
    
    m_DBTable.getCurPlayer( _nTableID, nCurPlayerID );
    
    if ( nCurPlayerID != _nPlayerID )
    {
        SGammonMsg sCmd;
        sCmd.m_chCmd = ANS_STEP;
        sCmd.m_nTableID = _nTableID;
        sCmd.m_chData = ( char ) P_NOT_VALID;
        
        sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );        
        return false;
        
    }
    
    return true;

}


/*!
    \fn CGammonTable::Run()
 */
int CGammonTable::Run()
{
    TVecUINT vecOpenTables;
    uint32_t nTime1, nTime2, nStepTime, nGameTime;
    while( 1 )
    {
        sleep( 1 );
        
        pthread_setcancelstate( PTHREAD_CANCEL_DISABLE, NULL );
        
        m_DBTable.selectFromStatus( ST_GAME, &vecOpenTables );
        
        for( TVecUINT::const_iterator i = vecOpenTables.begin(); i != vecOpenTables.end(); ++i )
        {
            m_DBTable.getPlayerStepTime( *i, nTime1 );
            m_DBTable.getPlayerGameTime( *i, nTime2 );
            m_DBTable.getTime2Step( *i, nStepTime );
            m_DBTable.getTime2Game( *i, nGameTime );
            
            if ( ( time( NULL) - nTime1 ) > nStepTime )
                endGame( *i, IGameLogic::TimeOut );
            
            if ( ( time( NULL) - nTime2 ) > nGameTime )
                endGame( *i, IGameLogic::TimeOut );
            
        }  
        pthread_setcancelstate( PTHREAD_CANCEL_ENABLE, NULL );
    }
}