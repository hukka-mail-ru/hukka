#ifndef CCHESSSERVICE_H_
#define CCHESSSERVICE_H_

#include <string.h>

#include "SqlChessTable.h"
#include "ChessLogic.h"

#include "../libs/gameserver/gamestructs.h"
#include "../libs/gameserver/GameService.h"


#define CH_FIELD_SIZE 71
#define CH_COLOR 69
#define CH_CUR_MOVE 70

#pragma pack(1)
struct ChessBigMsg: public GameMsgBase
{
	ChessBigMsg()
	{
		memset((void*)m_arField, 0, sizeof(m_arField) );
	}
	char     m_nPlayerNbr;
	uint32_t m_moveTime;
	uint32_t m_gameTime;
	uint8_t  m_arField[CH_FIELD_SIZE];
};
#pragma pack()

typedef GameService<ChessLogic> ChessServiseBase;

class ChessService : public ChessServiseBase
{

public:

	ChessService();

	virtual ~ChessService();

	void sendAnsStart(uint32_t _nTableID, uint32_t nPlayer1, uint32_t nPlayer2);

	uint8_t GetCurColor(const TVecChar* pField);

	void cmdGetField( uint32_t _nPlayerID, uint32_t _nTableID );

	SqlGameTable* GetSqlGameTable() { return &m_SqlChessTable; }

	enum Color { White = 0, Black };

private:

	SqlChessTable m_SqlChessTable;

};

#endif /*CCHESSSERVICE_H_*/
