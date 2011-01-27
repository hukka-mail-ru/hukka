#include "ChessService.h"
#include "../libs/gameserver/GameService.h"
#include "../libs/header/defservice.h"


#include "ChessLogic.h"

ChessService::ChessService() :
	ChessServiseBase("chs","*3F56B91ACCF798E83CC98141594C30420D26FE6B", "tbGhessRating", &m_SqlChessTable)
{
}

ChessService::~ChessService()
{
}

void ChessService::sendAnsStart(uint32_t _nTableID, uint32_t nPlayer1, uint32_t nPlayer2)
{
//TODO INIT FIELD!
    ChessBigMsg Msg;
    Msg.m_chCmd = ANS_START;
    Msg.m_nTableID = _nTableID;

    ChessLogic* logic = new ChessLogic(); // if move to after Msg.m_nTableID = _nTableID; - error ?

    std::copy(logic->GetPosForClient()->begin(),
    	       	  logic->GetPosForClient()->end(), (uint8_t*)&Msg.m_arField);

//    for ( int i = 0;  i <  sizeof(Msg.m_arField)/sizeof(Msg.m_arField[0]); ++i )
//        std::cout << (int) Msg.m_arField[i] << " ";
//    std::cout << "]" << std::endl;

    uint32_t nXPlayer = 0;
    m_SqlChessTable.getXPlayer(_nTableID, nXPlayer);

	Msg.m_nPlayerNbr = 0;
    Msg.m_arField[CH_COLOR] = nXPlayer;
    Msg.m_arField[CH_CUR_MOVE] = (nXPlayer == 0) ? 1 : 0;
    sendMsg( nPlayer1, &Msg, sizeof( Msg ) );

    /*
    std::cout << " ChessService::sendAnsStart: Msg.m_chCmd = " << (int) Msg.m_chCmd;
    std::cout << " Msg.m_nTableID = " << (int) Msg.m_nTableID << " ";
    std::cout << " Msg.m_nPlayerNbr = " << (int) Msg.m_nPlayerNbr << " " << std::endl;
    std::cout << " Msg.m_arField = [ " ;
      */

	Msg.m_nPlayerNbr = 1;
    Msg.m_arField[CH_COLOR] = nXPlayer;
    Msg.m_arField[CH_CUR_MOVE] = (nXPlayer == 1) ? 1 : 0;
    sendMsg( nPlayer2, &Msg, sizeof( Msg ) );

    delete logic;
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



