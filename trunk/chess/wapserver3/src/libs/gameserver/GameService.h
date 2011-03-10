#ifndef CGAMESERVICE_H_
#define CGAMESERVICE_H_

#include "../server/socketmanager.h"
#include "../server/accessinfo.h"
#include "../tools/playerselection.h"
#include "../tools/structs.h"
#include "../sql/sqlgametable.h"
#include "../sql/sqltableusers.h"
#include "../libs/header/defservice.h"
#include "gamestructs.h"
#include "../header/defserver.h"
#include "../header/deferror.h"



template<class TGameLogic> class GameService : public SocketManager, public AccessInfo
{
public:

	GameService( const char* _cszLogin, const char* _cszPwd, const char* _cszTableRatingName, SqlGameTable* _pSqlGameTable)
		: AccessInfo(_cszLogin, _cszPwd),
		  m_RatingTable( _cszTableRatingName, 1000 )
    {
		m_pPlayerSelection = new CPlayerSelection(_pSqlGameTable, &m_RatingTable);
    }

	virtual ~GameService()
	{
		delete m_pPlayerSelection;
	}

	virtual SqlGameTable* GetSqlGameTable() = 0;

	virtual void sendAnsStart(uint32_t _nTableID, uint32_t nPlayer1, uint32_t nPlayer2) = 0;

	virtual uint8_t GetCurColor(const TVecChar* pField) = 0;

	virtual void cmdGetField(uint32_t _nPlayerID, uint32_t _nTableID ) = 0;

protected:

	void sendMsg( uint32_t _nTO, void *_spCmd, int _nSize )
	{

	    TVecChar vecCmd;

	    vecCmd.assign( (char*)_spCmd, (char*)(_spCmd) + _nSize );

	    ClientMsg Msg;

	    Msg.InitMsg( _nTO, vecCmd );

	    m_pSocket->AddMsg( Msg );
	}

private:

	enum ECheckTime { TimeNotSet, NoTimeError, TimeOutError };

	enum EDrawState { no, offer, reject };

	void newMsg( ClientMsg *_pClientMsg )
	{
	    std::cout << "--- INCOMING MSG --- FROM: " << ( uint32_t )_pClientMsg->GetTo() <<
	                  ";  COMMAND: " << ( uint32_t ) _pClientMsg->GetCommand() <<
	                  ";  DATA: ";
		TVecChar vecCmd;
		_pClientMsg->GetData( ClientMsg::etpCommand, &vecCmd );
		for(TVecChar::const_iterator it = vecCmd.begin(); it != vecCmd.end(); ++it)
			std::cout << (uint32_t)(*it) << " ";
		std::cout << std::endl;

		if ( _pClientMsg->GetTo() == SRV )
		{
			if ( _pClientMsg->GetCommand() == 1 )
				if ( vecCmd[0] != NOERR )
				{
					/// @todo Закончить приложение
				}
				else
				{
					//syslog( LOG_INFO | LOG_LOCAL0, "started" );
				}

		}

		if ( _pClientMsg->GetTo() < 100 )
		{
			return;
		}

		char cmd = _pClientMsg->GetCommand();

		if( cmd == CMD_JOIN )
		{
			std::cout << "GameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_JOIN" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];

			cmdJoin( _pClientMsg->GetTo(), *nTableID );

		}
		else if( cmd == CMD_STEP )
		{
			std::cout << "GameService::newMsg from =" << _pClientMsg->GetTo() << " cmd = CMD_STEP" <<  std::endl;
			cmdStep( _pClientMsg->GetTo(), &vecCmd );
		}
		else if( cmd == CMD_GET_FIELD )
		{
			std::cout << "GameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_GET_FIELD" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];

			uint32_t _nPlayer = _pClientMsg->GetTo();

			cmdGetField( _nPlayer, *nTableID );

			sendDrawState( _nPlayer, *nTableID );

		}
		else if( cmd == CMD_LOOSE )
		{
			std::cout << "GameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_LOOSE" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];

			cmdLoose( *nTableID );

		}
        else if( cmd == CMD_TIMEOUT )
        {
            std::cout << "GameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_TIMEOUT" <<  std::endl;
            uint32_t *nTableID = (uint32_t*) &vecCmd[0];

            cmdTimeout( *nTableID );

        }
		else if( cmd == CMD_OPAGREE )
		{
			std::cout << "GameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_OPAGREE" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];

			cmdOpAgree( _pClientMsg->GetTo(), *nTableID );

		}
		else if( cmd == CMD_OPREJECT )
		{
			std::cout << "GameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_OPREJECT" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];

			cmdOpReject( _pClientMsg->GetTo(), *nTableID );

		}
		else if( cmd == CMD_DRAW )
		{
			std::cout << "GameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_DRAW" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];

			cmdDraw( _pClientMsg->GetTo(), *nTableID );
		}
		else if( cmd == CMD_DRAGREE )
		{
			std::cout << "GameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_DRAGREE" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];

			cmdDrAgree( _pClientMsg->GetTo(), *nTableID, vecCmd[sizeof(*nTableID)] );
		}
		else if ( cmd == CMD_CHECK_TIME )
		{
			std::cout << "GameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_CHECK_TIME" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];

			cmdCheckTime( _pClientMsg->GetTo(), *nTableID );
		}
		else
		{
			std::cout << "GameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = ??? " << (int)_pClientMsg->GetCommand() << std::endl;
		}
	}

	void DoAllMsg( MySocket * _pSocket )
	{
		m_pSocket = _pSocket;
		ClientMsg inMsg;

		while( _pSocket->GetMsg( inMsg ) )
		{
			newMsg( &inMsg );
		}
	}

	void OnClose( MySocket* ) {};

	void cmdJoin(uint32_t _nPlayerID, uint32_t _nTableID)
	{

		SGameMsg sCmd;
		sCmd.m_chCmd = ANS_JOIN;
		sCmd.m_nTableID = _nTableID;
		char strData;

		TVecChar vecCmd;
		ClientMsg Msg;

		std::cout << "GameService::cmdJoin _nPlayerID =" << _nPlayerID << " _nTableID " <<  _nTableID;

		if (joinToTable(_nPlayerID, _nTableID) )
		{
			std::cout << " : P_DONE";
			sCmd.m_chData = ( char ) P_DONE;
			sendOpponentToOwner(_nPlayerID, _nTableID);
		}
		else
		{
			std::cout << " : P_FAILED";
			sCmd.m_chData = ( char ) P_FAILED;
		}

		std::cout << std::endl;

		vecCmd.assign( (char*)&sCmd, (char*)(&sCmd)+sizeof(sCmd ));

		Msg.InitMsg(_nPlayerID, vecCmd);

		m_pSocket->AddMsg(Msg);
	}

	void sendOpponentToOwner(uint32_t _nPlayerID, uint32_t _nTableID)
	{

	    AnsOpponentMessage message;
	    message.cmd = ANS_OPPONENT;
	    message.tableID = _nTableID;
	    message.playerID = _nPlayerID;
	    message.rating = m_RatingTable.getRating( _nPlayerID );

	    std::cerr << "sendOpponentToOwner: " << message.tableID << " " << message.playerID <<
	               " " << message.rating << " size: " << sizeof(message) << std::endl;

	    uint32_t nPlayer0; // 0 = game Host

	    if ( GetSqlGameTable()->getIDPlayer0( _nTableID, nPlayer0 ) )
		{
			TVecChar vecCmd;
			ClientMsg Msg;

			vecCmd.assign( (char*)&message, (char*)(&message)+sizeof(message));

			// Get opponent name by ID
			SqlTableUsers wsUsers;
			TVecChar opponentName;
			wsUsers.GetUserName(_nPlayerID, &opponentName);

			vecCmd.reserve( vecCmd.size() + opponentName.size());
			vecCmd.insert( vecCmd.end(), opponentName.begin(), opponentName.end());

			Msg.InitMsg(nPlayer0, vecCmd);

			m_pSocket->AddMsg(Msg);
		}
	}

	void cmdOpAgree( uint32_t _nPlayerID, uint32_t _nTableID )
	{
		uint32_t nPlayer0 = 0;
		uint8_t nState = 0;

		SGameMsg sCmd;
	    sCmd.m_chCmd = ANS_OPAGREE_FAILED;
	    sCmd.m_nTableID = _nTableID;
	    sCmd.m_chData = (uint8_t) P_NOTALLOWED;

		if ( GetSqlGameTable()->getState(_nTableID, nState) )
		{
			if (nState == ST_FULL)
			{
				if(GetSqlGameTable()->getIDPlayer0( _nTableID, nPlayer0 ) &&
			       nPlayer0 == _nPlayerID )
				{
					startGame(_nTableID);
					return;
				}
			}
			else
			{
				sCmd.m_chData = (uint8_t) P_NOT_FULL;
			}
		}

		sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );
	}

	void cmdOpReject( uint32_t _nPlayerID, uint32_t _nTableID )
	{
		SGameMsg sCmd;
	    sCmd.m_chCmd = ANS_OPREJECT;
	    sCmd.m_nTableID = _nTableID;
		uint32_t nPlayer0 = 0;
		uint8_t nState = 0;
		if( GetSqlGameTable()->getIDPlayer0( _nTableID, nPlayer0 ) && nPlayer0 == _nPlayerID &&
		    GetSqlGameTable()->getState( _nTableID, nState ) && nState < ST_GAME )
		{
			uint32_t nPlayer1 = 0;
			sCmd.m_chData = P_DONE;
			if (GetSqlGameTable()->getIDPlayer1( _nTableID, nPlayer1 ))
			{
				sendMsg( nPlayer1, &sCmd, sizeof( sCmd ) );
			}

			GetSqlGameTable()->delIDPlayer1( _nTableID);
			GetSqlGameTable()->setState(_nTableID, ST_OPEN);
			sCmd.m_chData = P_DONE;
		}
		else
		    sCmd.m_chData = P_FAILED;

        sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );
	}

	bool joinToTable(uint32_t _nPlayerID, uint32_t _nTableID)
	{
		if ( !m_pPlayerSelection->checkContender(_nPlayerID, _nTableID) )
			return false;

		GetSqlGameTable()->setState(_nTableID, ST_FULL);

		GetSqlGameTable()->setIDPlayer1(_nTableID, _nPlayerID);

		//startGame(_nTableID);

		return true;
	}

	void startGame( uint32_t _nTableID)
	{
	    GetSqlGameTable()->setState( _nTableID, ST_GAME );

	    uint32_t nPlayer0 = 0;
	    uint32_t nPlayer1 = 0;
	    uint32_t nXPlayer = 0;
	    uint32_t nTime2Game = 0;
	    uint32_t nTime2Step = 0;

	    GetSqlGameTable()->getIDPlayer0( _nTableID, nPlayer0 );
	    GetSqlGameTable()->getIDPlayer1( _nTableID, nPlayer1 );
	    GetSqlGameTable()->getXPlayer( _nTableID, nXPlayer );

	    GetSqlGameTable()->setStepNum( _nTableID, 0 );

	    TGameLogic* GameLogic = new TGameLogic();

	    GetSqlGameTable()->updateFieldState( _nTableID, GameLogic->GetPosForDB() );

	    GetSqlGameTable()->setCurPlayer( _nTableID, nXPlayer );

	    time_t nStepTime = time( NULL );

	    GetSqlGameTable()->setPlayerStepTime( _nTableID, nStepTime );

		if( GetSqlGameTable()->getTime2Game( _nTableID, nTime2Game ) )
		{
			GetSqlGameTable()->setPlayerGameTime( _nTableID, 0, nTime2Game );
			GetSqlGameTable()->setPlayerGameTime( _nTableID, 1, nTime2Game );
		}

		std::cout << "GameService::startGame  sendAnsStart( _nTableID = " << _nTableID << ", nPlayer0 = " << nPlayer0 << ", nPlayer1 = " << nPlayer1 << std::endl;
	    sendAnsStart(_nTableID, nPlayer0, nPlayer1);

        delete GameLogic;
	}


	void cmdStep( uint32_t _nPlayerID, TVecChar* _vecStep )
	{
        //@TODO clarify situation with pointer! Mantis 25
	    TGameLogic *logic = new TGameLogic; // if move to befor if( isGoodPlayerID ) - error: isGoodPlayerID set to 0
		uint32_t nCurPlayer = 0;
	    uint32_t nPlayer0 = 0;
	    uint32_t nPlayer1 = 0;
	    uint32_t nXPlayer = 0;
	    uint8_t nState = 0;
	    uint8_t nDrawState = 0;

	    TVecChar vecFieldState;
	    const TVecChar *pField;
	    IGameLogic::StepRes Result = IGameLogic::NotValid;

	    uint32_t nTableID = *((uint32_t*)(&(*_vecStep)[0]));

        GetSqlGameTable()->getIDPlayer0( nTableID, nPlayer0 );
	    GetSqlGameTable()->getIDPlayer1( nTableID, nPlayer1 );
	    GetSqlGameTable()->getCurPlayer( nTableID, nCurPlayer );
    	GetSqlGameTable()->getDrawState( nTableID, nDrawState );

	    GetSqlGameTable()->getXPlayer( nTableID, nXPlayer );

	    bool isGoodPlayerID = 0;

	    //TODO refactoring! cmdEnd и cmdStep
	    if (nCurPlayer == 0/*White*/)
	    {
	    	if(nXPlayer == 0)
	    		isGoodPlayerID = _nPlayerID == nPlayer0;
	   		else
	    		isGoodPlayerID = _nPlayerID == nPlayer1;
	    }
	    else
	    {
	    	if(nXPlayer == 0)
	    		isGoodPlayerID = _nPlayerID == nPlayer1;
	   		else
	    		isGoodPlayerID = _nPlayerID == nPlayer0;
	    }

	    if ( GetSqlGameTable()->getState( nTableID, nState ) )
	    {
	    	isGoodPlayerID &= nState == ST_GAME;
	    }
		else
		{
			isGoodPlayerID = false;
		}

		ECheckTime checkTimeRes = NoTimeError;

	    if( isGoodPlayerID )
	    {

			checkTimeRes = checkTime( _nPlayerID, nTableID, false );

		    if (checkTimeRes == TimeOutError )
		    {
                delete logic;
		    	return;
		    }

	    	if (nDrawState == offer)
	    	{
                delete logic;
	    		return drawOffer(nTableID, _nPlayerID);
	    	}

		    TVecChar vecStep(_vecStep->size() - sizeof(nTableID));
		    std::copy(_vecStep->begin() + sizeof(nTableID), _vecStep->end(), vecStep.begin());

		    vecFieldState.reserve( 500 );

			std::cout << "GameService::cmdStep  nTableID = " << nTableID << ", nPlayer0 = " << nPlayer0 << ", nPlayer1 = " << nPlayer1 << std::endl;

		    if ( GetSqlGameTable()->getFieldState( nTableID, &vecFieldState ) )
		    {
		    	logic->SetPos( vecFieldState );

		        Result = logic->StepAnl( &vecStep );
		    }
		    else
		        Result = IGameLogic::NotValid;
	    }
	    else
	    {
	    	if (nDrawState == offer && nState == ST_GAME)
	        {
                delete logic;
	    		return drawWait(nTableID, _nPlayerID);
	    	}
	    	Result = IGameLogic::NotValid;
	    }

	    if ( Result == IGameLogic::Valid )
	    {
		    time_t nStepTime = time( NULL );

	    	if (checkTimeRes == NoTimeError)
	    	{
	    		setNewTime( nTableID, nCurPlayer, nStepTime  );
	    	}

		    GetSqlGameTable()->setPlayerStepTime( nTableID, nStepTime );


	    	GetSqlGameTable()->setDrawState( nTableID, no );

	        pField = logic->GetPosForDB();

	        GetSqlGameTable()->updateFieldState( nTableID, pField );

        	GetSqlGameTable()->setCurPlayer( nTableID, GetCurColor(pField) );

	        cmdGetField( nPlayer0, nTableID);
	        cmdGetField( nPlayer1, nTableID);

		    uint32_t nStep;
		    GetSqlGameTable()->getStepNum( nTableID, nStep );
		    ++nStep;
		    GetSqlGameTable()->setStepNum( nTableID, nStep );


	    }
	    else if ( Result == IGameLogic::NotValid )
	    {
   		    std::cout << "GameService::cmdStep NotValid nTableID = " << nTableID << ", _nPlayerID = " << _nPlayerID  << std::endl;

   		   	SGameMsg sCmd;
			sCmd.m_chCmd = ANS_STEP;
			sCmd.m_nTableID = nTableID;
			sCmd.m_chData = ( char ) P_NOT_VALID;
 		    sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );
	    	cmdGetField( _nPlayerID, nTableID);
            delete logic;
			return;
	    }
	    else
	    {
	        endGame( nTableID, Result, 0 );
	        // add SZ
            delete logic;
	        return; // ???
	    }

	}

	void setNewTime( uint32_t _nTableID, uint32_t nCurPlayer, time_t nStepTime )
	{
		uint32_t nTime2Game = 0;

		bool isTime2GameSet = GetSqlGameTable()->getTime2Game( _nTableID, nTime2Game );

		if (isTime2GameSet)
		{
			uint32_t nStepTimeOld = 0;
			uint32_t nGameTime = 0;
			GetSqlGameTable()->getPlayerStepTime( _nTableID, nStepTimeOld );
			GetSqlGameTable()->getPlayerGameTime( _nTableID, nCurPlayer, nGameTime );
			uint32_t nTime = nStepTime - nStepTimeOld;

			if( nTime > nGameTime )
			{
				return endGame( _nTableID, IGameLogic::TimeOut );
			}

			std::cout << "GameService::setNewTime new GameTime nTableID = " << _nTableID << ", nTime = " << nTime << ", nGameTime = " << nGameTime << ", nCurPlayer = " << nCurPlayer << std::endl;

			GetSqlGameTable()->setPlayerGameTime( _nTableID, nCurPlayer, nGameTime - nTime );
		}
	}

	void cmdLoose( uint32_t _nTableID )
	{
        endGame( _nTableID, IGameLogic::Loose );
	}

    void cmdTimeout( uint32_t _nTableID )
    {
        endGame( _nTableID, IGameLogic::TimeOut );
    }

	void endGame( uint32_t _nTableID, IGameLogic::StepRes _Result, uint32_t nMinStepCountForRating = 5 )
	{
	    uint32_t nCurPlayer = 0;
	    uint32_t nPlayer0   = 0;
	    uint32_t nPlayer1   = 0;
        uint32_t nStepNum   = 0;
        uint32_t nXPlayer   = 0;

	    SGameMsg sCmd0, sCmd1;
	    sCmd0.m_chCmd = sCmd1.m_chCmd = ANS_END;
	    sCmd0.m_nTableID = sCmd1.m_nTableID = _nTableID;
	    sCmd0.m_chData = sCmd1.m_chData = (char) ST_NO_RES;

	    GetSqlGameTable()->getIDPlayer0( _nTableID, nPlayer0 );
	    GetSqlGameTable()->getIDPlayer1( _nTableID, nPlayer1 );
	    GetSqlGameTable()->getCurPlayer( _nTableID, nCurPlayer );
	    GetSqlGameTable()->getStepNum( _nTableID, nStepNum );
  	    GetSqlGameTable()->getXPlayer( _nTableID, nXPlayer );

  	    cerr << "nPlayer0 " << nPlayer0 << endl;
        cerr << "nPlayer1 " << nPlayer1 << endl;
        cerr << "nCurPlayer " << nCurPlayer << endl;
        cerr << "nStepNum " << nStepNum << endl;
        cerr << "nXPlayer " << nXPlayer << endl;


	    //TODO refactoring! cmdEnd, cmdStep cmd Draw
        if (nCurPlayer == 0/*White*/)
	    {
            nCurPlayer = (nXPlayer == 0) ? nPlayer0 : nPlayer1;
	    }
	    else
	    {
            nCurPlayer = (nXPlayer == 0) ? nPlayer1 : nPlayer0;
	    }

        cerr << "OK. nCurPlayer " << nCurPlayer << endl;

		if ( nStepNum < nMinStepCountForRating)
		{
			sCmd1.m_chData = sCmd0.m_chData = (char) ST_NO_RES;
            GetSqlGameTable()->setState( _nTableID, ST_NO_RES );
        }
	    else if ( _Result == IGameLogic::Win )
	    {
	        if ( nCurPlayer == nPlayer0 )
	        {
	            sCmd0.m_chData = (char) P_WIN;
	            sCmd1.m_chData = (char) P_LOOSE;
	            GetSqlGameTable()->setState( _nTableID, ST_WIN_X );
	            setRating( nPlayer0, nPlayer1 );
	        }
	        else
	        {
	            sCmd0.m_chData = ( char ) P_LOOSE;
	            sCmd1.m_chData = ( char ) P_WIN;
	            GetSqlGameTable()->setState( _nTableID, ST_WIN_0 );
	            setRating( nPlayer1, nPlayer0 );
	        }

	    }
	    else if ( _Result == IGameLogic::Loose )
	    {
	        if ( nCurPlayer == nPlayer0 )
	        {
	            cerr << "OK. nCurPlayer == nPlayer0 "  << endl;

	            sCmd0.m_chData = ( char ) P_LOOSE;
	            sCmd1.m_chData = ( char ) P_WIN;
	            GetSqlGameTable()->setState( _nTableID, ST_WIN_0 );
	            setRating( nPlayer1, nPlayer0 );
	        }
	        else
	        {
	            cerr << "OK. nCurPlayer != nPlayer0 " << nCurPlayer << endl;

	            sCmd0.m_chData = (char) P_WIN;
	            sCmd1.m_chData = (char) P_LOOSE;
	            GetSqlGameTable()->setState( _nTableID, ST_WIN_X );
	            setRating( nPlayer0, nPlayer1 );
	        }

	    }
	    else if ( _Result == IGameLogic::Draw )
	    {
            sCmd0.m_chData = sCmd1.m_chData = ( char ) P_DRAW;
            GetSqlGameTable()->setState( _nTableID, ST_DRAW );
            setRatingDraw( nPlayer1, nPlayer0 );
	    }
	    else if ( _Result == IGameLogic::TimeOut )
	    {
	        if ( nCurPlayer == nPlayer0 )
	        {
	            sCmd0.m_chData = ( char ) P_LOOSE_TIME;
	            sCmd1.m_chData = ( char ) P_WIN_TIME;
	            GetSqlGameTable()->setState( _nTableID, ST_WIN_0 );
	            setRating( nPlayer1, nPlayer0 );
	        }
	        else
	        {
	            sCmd0.m_chData = (char) P_WIN_TIME;
	            sCmd1.m_chData = (char) P_LOOSE_TIME;
	            GetSqlGameTable()->setState( _nTableID, ST_WIN_X );
	            setRating( nPlayer0, nPlayer1 );
	        }
	    }
	    else
	    {
			std::cout << "GameService::endGame  ERROR: unknown _Result = " << _Result << std::endl;
	    }

	    sendMsg( nPlayer0, &sCmd0, sizeof( sCmd0 ) );
	    sendMsg( nPlayer1, &sCmd1, sizeof( sCmd1 ) );

	}

	void setRating( uint32_t _nWinnerID, uint32_t _nLooserID )
	{
		//TODO
	    uint32_t nWinnerRating = m_RatingTable.getRating( _nWinnerID );
	    uint32_t nLooserRating = m_RatingTable.getRating( _nLooserID );
	    uint32_t nTmp = nWinnerRating;

	    std::cout << "GameService::setRating(). nWinnerRating " <<  nWinnerRating << std::endl;

	    nWinnerRating += (uint32_t)(nLooserRating * 0.1);
	    nLooserRating -= (uint32_t)(nLooserRating * 0.1);

	    std::cout << "GameService::setRating(). nLooserRating * 0.1 " <<  (uint32_t)(nLooserRating * 0.1) << std::endl;
	    std::cout << "GameService::setRating(). nWinnerRating " <<  nWinnerRating << std::endl;
	    std::cout << "GameService::setRating(). nLooserRating " <<  nLooserRating << std::endl;

	    m_RatingTable.setRating( _nWinnerID, nWinnerRating );
	    m_RatingTable.setRating( _nLooserID, nLooserRating );
	}

	void setRatingDraw( uint32_t _nPlayer0, uint32_t _nPlayer1 )
	{
	    uint32_t nRating0 = m_RatingTable.getRating( _nPlayer0 );
	    uint32_t nRating1 = m_RatingTable.getRating( _nPlayer1 );

	    nRating0 += (uint32_t)(((double)(nRating1) - (double)(nRating0)) * 0.1);
	    nRating1 += (uint32_t)(((double)(nRating0) - (double)(nRating1)) * 0.1);

	    m_RatingTable.setRating( _nPlayer0, nRating0 );
	    m_RatingTable.setRating( _nPlayer1, nRating1 );
	}

	void cmdDraw(uint32_t _nPlayerID, uint32_t _nTableID)
	{
		uint32_t nCurPlayer = 0;
	    uint32_t nPlayer0 = 0;
	    uint32_t nPlayer1 = 0;
	    uint32_t nXPlayer = 0;
		uint8_t nDrawState = 0;

        GetSqlGameTable()->getIDPlayer0( _nTableID, nPlayer0 );
	    GetSqlGameTable()->getIDPlayer1( _nTableID, nPlayer1 );
	    GetSqlGameTable()->getCurPlayer( _nTableID, nCurPlayer );
	    GetSqlGameTable()->getXPlayer( _nTableID, nXPlayer );
	    GetSqlGameTable()->getDrawState( _nTableID, nDrawState );

	    bool isGoodPlayerID = 0;

	    //TODO refactoring! cmdEnd и cmdStep
	    if (nCurPlayer == 0/*White*/)
	    {
	    	if(nXPlayer == 0)
	    		isGoodPlayerID = _nPlayerID == nPlayer0;
	   		else
	    		isGoodPlayerID = _nPlayerID == nPlayer1;
	    }
	    else
	    {
	    	if(nXPlayer == 0)
	    		isGoodPlayerID = _nPlayerID == nPlayer1;
	   		else
	    		isGoodPlayerID = _nPlayerID == nPlayer0;
	    }


	    if( isGoodPlayerID )
		{
	    	if (nDrawState == offer)
		    {
	    		return drawWait(_nTableID, _nPlayerID);
	    	}
	    	else if (nDrawState == reject)
	    	{
	    		sendAscDraw(_nTableID, _nPlayerID, P_NOTALLOWED);
	    	}
	    	else
	    	{
	        	GetSqlGameTable()->setCurPlayer( _nTableID, nCurPlayer == 0 ? 1 : 0 );
	        	GetSqlGameTable()->setDrawState( _nTableID, offer );
	        	drawWait(_nTableID, _nPlayerID);
	        	drawOffer(_nTableID, _nPlayerID == nPlayer0? nPlayer1 : nPlayer0);
	    	}
		}
	    else
	    {
    		sendAscDraw(_nTableID, _nPlayerID, P_NOTALLOWED);
	    }

	}

	void cmdDrAgree( uint32_t _nPlayerID, uint32_t _nTableID, uint8_t value )
	{
		uint32_t nCurPlayer = 0;
	    uint32_t nPlayer0 = 0;
	    uint32_t nPlayer1 = 0;
	    uint32_t nXPlayer = 0;
		uint8_t nDrawState = 0;

        GetSqlGameTable()->getIDPlayer0( _nTableID, nPlayer0 );
	    GetSqlGameTable()->getIDPlayer1( _nTableID, nPlayer1 );
	    GetSqlGameTable()->getCurPlayer( _nTableID, nCurPlayer );
	    GetSqlGameTable()->getXPlayer( _nTableID, nXPlayer );
	    GetSqlGameTable()->getDrawState( _nTableID, nDrawState );

	    bool isGoodPlayerID = 0;

	    //TODO refactoring! cmdEnd и cmdStep
	    if (nCurPlayer == 0/*White*/)
	    {
	    	if(nXPlayer == 0)
	    		isGoodPlayerID = _nPlayerID == nPlayer0;
	   		else
	    		isGoodPlayerID = _nPlayerID == nPlayer1;
	    }
	    else
	    {
	    	if(nXPlayer == 0)
	    		isGoodPlayerID = _nPlayerID == nPlayer1;
	   		else
	    		isGoodPlayerID = _nPlayerID == nPlayer0;
	    }

	    if( isGoodPlayerID )
		{
	    	if (nDrawState == offer)
		    {
				if (value == 'Y')
				{
			    	GetSqlGameTable()->setCurPlayer( _nTableID, nCurPlayer == 0 ? 1 : 0 );
		        	GetSqlGameTable()->setDrawState( _nTableID, 0 );
		        	GetSqlGameTable()->setState( _nTableID, ST_DRAW );
		    		sendAscDraw(_nTableID, nPlayer0, P_ACCEPT);
		    		sendAscDraw(_nTableID, nPlayer1, P_ACCEPT);
		    		endGame(_nTableID, IGameLogic::Draw, 0);
		    	}
		    	else if (value == 'N')
		    	{
			    	GetSqlGameTable()->setCurPlayer( _nTableID, nCurPlayer == 0 ? 1 : 0 );
		        	GetSqlGameTable()->setDrawState( _nTableID, reject );
		    		sendAscDraw(_nTableID, nPlayer0, P_REJECT);
		    		sendAscDraw(_nTableID, nPlayer1, P_REJECT);
		    	}
		    	else
		    	{
		    		sendAscDraw(_nTableID, _nPlayerID, P_OFFER);
		    	}
		    }
	    	else
	    	{
	    		sendAscDraw(_nTableID, _nPlayerID, P_NOTALLOWED);
	    	}
		}
	    else
	    {
    		sendAscDraw(_nTableID, _nPlayerID, P_NOTALLOWED);
	    }
	}

	void sendDrawState( uint32_t _nPlayerID, uint32_t nTableID )
	{
		uint8_t nDrawState = 0;
	    GetSqlGameTable()->getDrawState( nTableID, nDrawState );

    	if (nDrawState == offer)
	    {
    		uint32_t nCurPlayer = 0;
    	    uint32_t nPlayer0 = 0;
    	    uint32_t nPlayer1 = 0;
    	    uint32_t nXPlayer = 0;

            GetSqlGameTable()->getIDPlayer0( nTableID, nPlayer0 );
    	    GetSqlGameTable()->getIDPlayer1( nTableID, nPlayer1 );
    	    GetSqlGameTable()->getXPlayer( nTableID, nXPlayer );
    	    GetSqlGameTable()->getCurPlayer( nTableID, nCurPlayer );

    	    bool isGoodPlayerID = 0;
    	    //TODO refactoring! cmdEnd и cmdStep
    	    if (nCurPlayer == 0/*White*/)
    	    {
    	    	if(nXPlayer == 0)
    	    		isGoodPlayerID = _nPlayerID == nPlayer0;
    	   		else
    	    		isGoodPlayerID = _nPlayerID == nPlayer1;
    	    }
    	    else
    	    {
    	    	if(nXPlayer == 0)
    	    		isGoodPlayerID = _nPlayerID == nPlayer1;
    	   		else
    	    		isGoodPlayerID = _nPlayerID == nPlayer0;
    	    }
    	    if( isGoodPlayerID )
    		{
    	    	drawOffer(nTableID, _nPlayerID);
    		}
    	    else
    	    {
    	    	drawWait(nTableID, _nPlayerID);
    	    }
	    }

	}

	void drawOffer(uint32_t _nTableID, uint32_t _nPlayerID)
	{
		sendAscDraw(_nTableID, _nPlayerID, P_OFFER);
	}

	void drawWait(uint32_t _nTableID, uint32_t _nPlayerID)
	{
		sendAscDraw(_nTableID, _nPlayerID, P_WAIT);
	}

	void sendAscDraw(uint32_t _nTableID, uint32_t _nPlayerID, uint8_t _nDrawStatus)
	{
		SGameMsg sCmd;
	    sCmd.m_chCmd = ANS_DRAW;
	    sCmd.m_nTableID = _nTableID;
	    sCmd.m_chData = _nDrawStatus;

        sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );
	}

	ECheckTime checkTime( uint32_t _nPlayerID, uint32_t _nTableID, bool isSendTime )
	{
		uint32_t nTime2Step = 0;
		uint32_t nTime2Game = 0;
		uint32_t nCurPlayer = 0;
		uint32_t nStepTime = 0;
		bool isTime2StepSet = GetSqlGameTable()->getTime2Step( _nTableID, nTime2Step );
		bool isTime2GameSet = GetSqlGameTable()->getTime2Game( _nTableID, nTime2Game );

		GetSqlGameTable()->getCurPlayer( _nTableID, nCurPlayer );
		GetSqlGameTable()->getPlayerStepTime( _nTableID, nStepTime );

        time_t nCurTime = time( NULL );
		uint32_t nTime = nCurTime - nStepTime;

		if( isTime2StepSet )
		{
			if( nTime > nTime2Step)
			{
				endGame( _nTableID, IGameLogic::TimeOut );
				std::cout << "GameService::checkTime Step TimeOut - END GAME! nTableID = " << _nTableID << ", nTime = " << nTime << ", nTime2Step = " << nTime2Step << std::endl;
				return 	TimeOutError;
			}
			else if (isSendTime)
			{
				SNGameMsg sCmd;
			    sCmd.m_chCmd = ANS_CHECK_TIME_STEP;
			    sCmd.m_nTableID = _nTableID;
			    sCmd.m_nData = nTime2Step - nTime;

			    std::cout << "GameService::checkTime ANS_CHECK_TIME_STEP nTableID = " << _nTableID
			              << ", nTime = " << nTime << ", sCmd.m_nData = " << sCmd.m_nData << std::endl;

		        sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );

		        // NOT SO FAST, BABY!
		        // Wee need this because ANS_CHECK_TIME_STEP and ANS_CHECK_TIME_GAME can glue together
		        // and the client won't parse it.
		        usleep(200000);
			}

		}

		if ( isTime2GameSet )
		{
			uint32_t nGameTime = 0;
			GetSqlGameTable()->getPlayerGameTime( _nTableID, nCurPlayer, nGameTime );
			if( nTime > nGameTime )
			{
				endGame( _nTableID, IGameLogic::TimeOut );
				std::cout << "GameService::checkTime Game TimeOut - END GAME! nTableID = " << _nTableID << ", nTime = " << nTime << ", nGameTime = " << nGameTime << std::endl;
				return 	TimeOutError;
			}
			else if (isSendTime)
			{
				SNGameMsg sCmd;
			    sCmd.m_chCmd = ANS_CHECK_TIME_GAME;
			    sCmd.m_nTableID = _nTableID;
			    sCmd.m_nData = nGameTime - nTime;
		        sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );
			}
		}

		return (!isTime2StepSet && !isTime2GameSet) ? TimeNotSet : NoTimeError;

	}

	void cmdCheckTime( uint32_t _nPlayerID, uint32_t _nTableID )
	{
		if ( checkTime( _nPlayerID, _nTableID, true ) == TimeNotSet )
		{
			GameMsgBase sCmd;
		    sCmd.m_chCmd = ANS_CHECK_TIME_NOT_SET;
		    sCmd.m_nTableID = _nTableID;
	        sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );
		}
	}

private:

	CPlayerSelection	*m_pPlayerSelection;

	CSqlRatingTable		m_RatingTable;

	MySocket			*m_pSocket;

};

#endif /*CGAMESERVICE_H_*/
