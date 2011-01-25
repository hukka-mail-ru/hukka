#ifndef CCHESSSERVICE_H_
#define CCHESSSERVICE_H_

#include <string.h>

#include "SqlChessTable.h"
#include "ChessLogic.h"

#include "../libs/gameserver/gamestructs.h"
#include "../libs/gameserver/CGameService.h"


#define CH_FIELD_SIZE 66
#define CH_COLOR 64
#define CH_CUR_MOVE 65

#pragma pack(1)
struct SChessBigMsg: public SGameMsgBase
{
	SChessBigMsg()
	{
		memset((void*)m_arField, 0, sizeof(m_arField) );
	}
	char    m_nPlayerNbr; 
	uint8_t m_arField[CH_FIELD_SIZE];
};
#pragma pack()

typedef CGameService<ChessLogic> TChessServiseBase;

class CChessService : public TChessServiseBase
{
	
public:

	CChessService();
	
	virtual ~CChessService();
	
	void sendAnsStart(uint32_t _nTableID, uint32_t nPlayer1, uint32_t nPlayer2);
	
	uint8_t GetCurColor(const TVecChar* pField);

	void cmdGetField( uint32_t _nPlayerID, uint32_t _nTableID );
	
	CSqlGameTable* GetSqlGameTable() { return &m_SqlChessTable; }
	
	enum Color { White = 0, Black };
	
private:
	
	CSqlChessTable m_SqlChessTable;
	
};

#endif /*CCHESSSERVICE_H_*/
