#ifndef GAMESTRUCTS_H_
#define GAMESTRUCTS_H_

/*
	Структура исходящего короткого сообщения типа КОМАНДА <TableID> <Параметр>
*/
#pragma pack(1)
struct GameMsgBase
{
	uint8_t m_chCmd;
	uint32_t m_nTableID;
};
struct SGameMsg : public GameMsgBase
{
	uint8_t m_chData;
};

struct SNGameMsg : public GameMsgBase
{
	uint32_t m_nData;
};

struct AnsOpponentMessage
{
    uint8_t  cmd;
    uint32_t tableID;
    uint32_t playerID;
    uint32_t rating;
};

struct AnsEndMessage: public SGameMsg
{
    uint32_t rating;
};

#pragma pack()
#endif /*GAMESTRUCTS_H_*/
