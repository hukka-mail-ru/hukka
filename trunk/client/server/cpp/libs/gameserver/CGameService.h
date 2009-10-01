#ifndef CGAMESERVICE_H_
#define CGAMESERVICE_H_

#include "../server/socketmanager.h"
#include "../server/accessinfo.h"
#include "../tools/playerselection.h"
#include "../tools/structs.h"
#include "../sql/sqlgametable.h"
#include "gamedefs.h"
#include "gamestructs.h"
#include "../header/defserver.h"
#include "../header/deferror.h"

template<class TGameLogic> class CGameService : public CSocketManager, public CAccessInfo
{
public:
	
	CGameService( const char* _cszLogin, const char* _cszPwd, const char* _cszTableRatingName, CSqlGameTable* _pSqlGameTable)	
		: CAccessInfo(_cszLogin, _cszPwd), 
		  m_RatingTable( _cszTableRatingName, 1000 )
    {
		m_pPlayerSelection = new CPlayerSelection(_pSqlGameTable, &m_RatingTable);
    } 
	
	virtual ~CGameService()
	{
		delete m_pPlayerSelection;
	}
	
	virtual CSqlGameTable* GetSqlGameTable() = 0;
	
	virtual void sendAnsStart(uint32_t _nTableID, uint32_t nPlayer1, uint32_t nPlayer2) = 0;
	
	virtual uint8_t GetCurColor(const TVecChar* pField) = 0;
	
	virtual void cmdGetField(uint32_t _nPlayerID, uint32_t _nTableID ) = 0;

protected:
	
	void sendMsg( uint32_t _nTO, void *_spCmd, int _nSize )
	{
	    TVecChar vecCmd;
	    
	    vecCmd.assign( (char*)_spCmd, (char*)(_spCmd) + _nSize );
	    
	    CClientMsg Msg;
	    
	    Msg.InitMsg( _nTO, vecCmd );
	    
	    m_pSocket->AddMsg( Msg );
	}
	
private:

	void newMsg( CClientMsg *_pClientMsg )
	{
		std::cout << "CGameService::newMsg..." <<  std::endl;
		TVecChar vecCmd;
		_pClientMsg->GetData( CClientMsg::etpCommand, &vecCmd );
		
		std::cout << "CGameService::newMsg vecCmd[" << vecCmd.size() << "] " << std::endl;
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
			std::cout << "CGameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_JOIN" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];
			
			cmdJoin( _pClientMsg->GetTo(), *nTableID );
			
		}
		else if( cmd == CMD_STEP )
		{		
			std::cout << "CGameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_STEP" <<  std::endl;
			cmdStep( _pClientMsg->GetTo(), &vecCmd );
		}
		else if( cmd == CMD_GET_FIELD )
		{
			std::cout << "CGameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_GET_FIELD" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];
				
			cmdGetField( _pClientMsg->GetTo(), *nTableID );
				
		}
		else if( cmd == CMD_LOOSE )
		{
			std::cout << "CGameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_LOOSE" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];
				
			cmdLoose( _pClientMsg->GetTo(), *nTableID );
				
		}
		else if( cmd == CMD_OPAGREE )
		{
			std::cout << "CGameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_OPAGREE" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];
			
			cmdOpAgree( _pClientMsg->GetTo(), *nTableID );
				
		}
		else if( cmd == CMD_OPREJECT )
		{
			std::cout << "CGameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = CMD_OPREJECT" <<  std::endl;
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];	
				
			cmdOpReject( _pClientMsg->GetTo(), *nTableID );
				
		}
		else	
		{
			std::cout << "CGameService::newMsg from = " << _pClientMsg->GetTo() << " cmd = ??? " << (int)_pClientMsg->GetCommand() << std::endl;
		}
	}

	void DoAllMsg( CMySocket * _pSocket )
	{
		m_pSocket = _pSocket;
		CClientMsg inMsg;
		
		while( _pSocket->GetMsg( inMsg ) )
		{
			newMsg( &inMsg );
		}
	}

	void OnClose( CMySocket* ) {};
	
	void cmdJoin(uint32_t _nPlayerID, uint32_t _nTableID) 
	{

		SGameMsg sCmd;
		sCmd.m_chCmd = ANS_JOIN;
		sCmd.m_nTableID = _nTableID;
		char strData;

		TVecChar vecCmd;
		CClientMsg Msg;

		std::cout << "CGameService::cmdJoin _nPlayerID =" << _nPlayerID << " _nTableID " <<  _nTableID;
		
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

		SNGameMsg sCmd;
		sCmd.m_chCmd = ANS_OPPONENT;
		sCmd.m_nTableID = _nTableID;
		sCmd.m_nData = _nPlayerID;
		
	    uint32_t nPlayer0;
	    
	    if ( GetSqlGameTable()->getIDPlayer0( _nTableID, nPlayer0 ) )
		{
			TVecChar vecCmd;
			CClientMsg Msg;
			
			vecCmd.assign( (char*)&sCmd, (char*)(&sCmd)+sizeof(sCmd ));
	
			Msg.InitMsg(nPlayer0, vecCmd);
	
			m_pSocket->AddMsg(Msg);
		}
	}
	
	void cmdOpAgree( uint32_t _nPlayerID, uint32_t _nTableID ) 
	{
		uint32_t nPlayer0;
		if ( GetSqlGameTable()->getIDPlayer0( _nTableID, nPlayer0 ) &&
		     nPlayer0 == _nPlayerID)
		{
			startGame(_nTableID);
		}	
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
	    
	    uint32_t nPlayer0 = 0, nPlayer1 = 0, nXPlayer = 0;
	    
	    GetSqlGameTable()->getIDPlayer0( _nTableID, nPlayer0 );
	    GetSqlGameTable()->getIDPlayer1( _nTableID, nPlayer1 );
	    GetSqlGameTable()->getXPlayer( _nTableID, nXPlayer );
	    
	    GetSqlGameTable()->setStepNum( _nTableID, 0 );

	    TGameLogic GameLogic;  
	    
	    GetSqlGameTable()->updateFieldState( _nTableID, GameLogic.GetPosForDB() );
	    
	    GetSqlGameTable()->setCurPlayer( _nTableID, nXPlayer );
	    
	    uint32_t nTime2Game, nTime2Step;
	    
	    time_t nStepTime = time( NULL );
	    
	    GetSqlGameTable()->setPlayerStepTime( _nTableID, nStepTime );

		std::cout << "CGameService::startGame  sendAnsStart( _nTableID = " << _nTableID << ", nPlayer0 = " << nPlayer0 << ", nPlayer1 = " << nPlayer1 << std::endl;
	    sendAnsStart(_nTableID, nPlayer0, nPlayer1);
	}


	void cmdStep( uint32_t _nPlayerID, TVecChar* _vecStep )
	{
	    TGameLogic logic; // if move to befor if( isGoodPlayerID ) - error: isGoodPlayerID set to 0  
		uint32_t nCurPlayer = 0;
	    uint32_t nPlayer0 = 0;
	    uint32_t nPlayer1 = 0;
	    uint32_t nXPlayer = 0;
	    uint8_t nState = 0;
	    
	    TVecChar vecFieldState; 
	    const TVecChar *pField;
	    IGameLogic::StepRes Result = IGameLogic::NotValid;
	    
	    uint32_t nTableID = *((uint32_t*)(&(*_vecStep)[0]));

        GetSqlGameTable()->getIDPlayer0( nTableID, nPlayer0 );
	    GetSqlGameTable()->getIDPlayer1( nTableID, nPlayer1 );
	    GetSqlGameTable()->getCurPlayer( nTableID, nCurPlayer );
	    
	    GetSqlGameTable()->getXPlayer( nTableID, nXPlayer );
	    
	    uint8_t isGoodPlayerID = 0;
	    
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
		
	    
	    if( isGoodPlayerID )
	    {
		    TVecChar vecStep(_vecStep->size() - sizeof(nTableID));
		    std::copy(_vecStep->begin() + sizeof(nTableID), _vecStep->end(), vecStep.begin()); 
		        
		    vecFieldState.reserve( 500 );
	
			std::cout << "CGameService::cmdStep  nTableID = " << nTableID << ", nPlayer0 = " << nPlayer0 << ", nPlayer1 = " << nPlayer1 << std::endl;	        
		    
		    if ( GetSqlGameTable()->getFieldState( nTableID, &vecFieldState ) )
		    {
		    	logic.SetPos( vecFieldState );
		    
		        Result = logic.StepAnl( &vecStep );
		    }
		    else
		        Result = IGameLogic::NotValid;
	    }
	    
	    if ( Result == IGameLogic::Valid )
	    {
	        pField = logic.GetPosForDB();

	        GetSqlGameTable()->updateFieldState( nTableID, pField );

        	GetSqlGameTable()->setCurPlayer( nTableID, GetCurColor(pField) );
	        
	        cmdGetField( nPlayer0, nTableID);
	        cmdGetField( nPlayer1, nTableID);
	        
		    uint32_t nStep;   
		    GetSqlGameTable()->getStepNum( nTableID, nStep );
		    ++nStep;
		    GetSqlGameTable()->setStepNum( nTableID, nStep );

		    time_t nStepTime = time( NULL );
		    
		    GetSqlGameTable()->setPlayerStepTime( nTableID, nStepTime );

	    }    
	    else if ( Result == IGameLogic::NotValid )
	    {
   		    std::cout << "CGameService::cmdStep NotValid nTableID = " << nTableID << ", _nPlayerID = " << _nPlayerID  << std::endl;
   		    
   		   	SGameMsg sCmd;
			sCmd.m_chCmd = ANS_STEP;
			sCmd.m_nTableID = nTableID;
			sCmd.m_chData = ( char ) P_NOT_VALID;
 		    sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );	        
	    	cmdGetField( _nPlayerID, nTableID);
			return;
	    }
	    else
	    {
	        endGame( nTableID, Result );
	        // add SZ
	        return; // ??? 
	    } 
	    
	}

	void cmdLoose( uint32_t _nPlayerID, uint32_t _nTableID )
	{
        endGame( _nTableID, IGameLogic::Loose );
	}
	
	void endGame( uint32_t _nTableID, IGameLogic::StepRes _Result )
	{
		const uint32_t nMinStepCountForRating = 5;
		
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

        if (nCurPlayer == 0/*White*/)
	    {
	    	if(nXPlayer == 0)
	    		nCurPlayer == nPlayer0;
	   		else
	    		nCurPlayer == nPlayer1;
	    }
	    else
	    {
	    	if(nXPlayer == 0)
	    		nCurPlayer == nPlayer1;
	   		else
	    		nCurPlayer == nPlayer0;
	    }
	    
		if ( nStepNum < nMinStepCountForRating)
		{
			sCmd1.m_chData = sCmd0.m_chData = (char) ST_NO_RES;
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
	            sCmd0.m_chData = ( char ) P_LOOSE;
	            sCmd1.m_chData = ( char ) P_WIN;
	            GetSqlGameTable()->setState( _nTableID, ST_WIN_0 );
	            setRating( nPlayer1, nPlayer0 );
	        }
	        else
	        {
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
	            sCmd1.m_chData = ( char ) P_WIN;
	            GetSqlGameTable()->setState( _nTableID, ST_WIN_0 );
	            setRating( nPlayer1, nPlayer0 );
	        }
	        else
	        {
	            sCmd0.m_chData = (char) P_WIN; 
	            sCmd1.m_chData = (char) P_LOOSE_TIME;
	            GetSqlGameTable()->setState( _nTableID, ST_WIN_X );
	            setRating( nPlayer0, nPlayer1 );
	        }        
	    }
	    else    
	    {
			std::cout << "CGameService::endGame  ERROR: unknown _Result = " << _Result << std::endl;	    	
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
	    
	    nWinnerRating += (uint32_t)(nLooserRating * 0.1);
	    nLooserRating -= (uint32_t)(nLooserRating * 0.1);    
	    
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
	
private:
	
	CPlayerSelection	*m_pPlayerSelection;
	
	CSqlRatingTable		m_RatingTable;
	
	CMySocket			*m_pSocket;
	
};

#endif /*CGAMESERVICE_H_*/
