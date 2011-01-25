#ifndef GAMESTRUCTS_H_
#define GAMESTRUCTS_H_

/*
	Структура исходящего короткого сообщения типа КОМАНДА <TableID> <Параметр>
*/
#pragma pack(1)
struct SGameMsgBase
{
	uint8_t m_chCmd;
	uint32_t m_nTableID;
};
struct SGameMsg : public SGameMsgBase
{
	uint8_t m_chData;
};

struct SNGameMsg : public SGameMsgBase
{
	uint32_t m_nData;
};
#pragma pack()
#endif /*GAMESTRUCTS_H_*/
