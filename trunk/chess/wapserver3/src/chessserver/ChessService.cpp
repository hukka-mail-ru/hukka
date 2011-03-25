#include "ChessService.h"
#include "../libs/gameserver/GameService.h"
#include "../libs/header/defservice.h"


#include "ChessLogic.h"

ChessService::ChessService() :
	ChessServiseBase("chs","*3F56B91ACCF798E83CC98141594C30420D26FE6B", "tbChessRating", &m_SqlChessTable)
{
}

ChessService::~ChessService()
{
}

void ChessService::sendAnsStart(uint32_t _nTableID, uint32_t nPlayer1, uint32_t nPlayer2)
{
    GameMsgBase msg;
    msg.m_chCmd = ANS_START;
    msg.m_nTableID = _nTableID;

    sendMsg( nPlayer1, &msg, sizeof( msg ) );
    sendMsg( nPlayer2, &msg, sizeof( msg ) );
}

void ChessService::cmdGetField( uint32_t _nPlayerID, uint32_t _nTableID )
{

    TVecChar vecField;
    const TVecChar *pvecMsgField;
    uint32_t nPlayer0 = 0, nCurPlayer = 0;

    ChessBigMsg sCmd;
    m_SqlChessTable.getIDPlayer0( _nTableID, nPlayer0 );

	if ( nPlayer0 == 0 || _nPlayerID == nPlayer0)
		sCmd.m_nPlayerNbr = 0;
	else
		sCmd.m_nPlayerNbr = 1;

    sCmd.m_chCmd = ANS_FIELD;
    sCmd.m_nTableID = 0;


    uint32_t moveTime = 0;
    uint32_t gameTime = 0;
    if ( checkTime( _nPlayerID, _nTableID, false, moveTime, gameTime ) == NoTimeError )
    {
        sCmd.m_moveTime = moveTime;
        sCmd.m_gameTime = gameTime;
    }
    else
    {
        sCmd.m_moveTime = INVALID_TIME;
        sCmd.m_gameTime = INVALID_TIME;
    }


    uint8_t nState = 0;

    if ( m_SqlChessTable.getState( _nTableID, nState ) )
	{
	    if ( nState == ST_GAME && m_SqlChessTable.getFieldState( _nTableID, &vecField ) )
	    {
	    	sCmd.m_nTableID = _nTableID;

	    	ChessLogic* logic = new ChessLogic();

	        logic->SetPos(vecField);

	        pvecMsgField = logic->GetPosForClient();

	        int k = 0;

	        for ( TVecChar::const_iterator i = pvecMsgField->begin(); i != pvecMsgField->end(); ++i )
	        {
	            sCmd.m_arField[k] = *i;
	            ++k;
	        }
	        uint32_t nXPlayer = 0;

	        m_SqlChessTable.getXPlayer(_nTableID, nXPlayer);

	        sCmd.m_arField[CH_COLOR] = nXPlayer;

	        m_SqlChessTable.getCurPlayer(_nTableID, nCurPlayer);

    		if( nCurPlayer == White )
    		{
	        	sCmd.m_arField[CH_CUR_MOVE] =  sCmd.m_nPlayerNbr == nXPlayer;
    		}
    		else //Black
    		{
    			sCmd.m_arField[CH_CUR_MOVE] =  sCmd.m_nPlayerNbr != nXPlayer;
    		}

            delete logic;

	    }
	}

    sendMsg( _nPlayerID, &sCmd, sizeof( sCmd ) );


}

uint8_t ChessService::GetCurColor(const TVecChar* pField)
{
	SPosition pos;

	std::copy(pField->begin(), pField->end(), (uint8_t*)&pos);

	return pos.w_turn ? 0 : 1;
}



