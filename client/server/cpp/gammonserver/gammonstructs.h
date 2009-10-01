#ifndef GAMMONSTRUCTS_H
#define GAMMONSTRUCTS_H

#include <stdint.h>

struct SGammonStep
{
	uint32_t	m_nTableID;
  //uint8_t		m_arnStep[8];

// SZ:
  
  /// Move description
  //  Comes in pairs, from point @arg{desc}[2*@arg{i}] to
  //  @arg{desc}[2*@arg{i}+1] (@eqn{0 <= i <= 4}).
  //
  //  25 is bar, 0 is home.
  //
  //  First unused position, if any, should be set to -1.
    uint8_t		m_arnStep[8];

    // m_btSide == 0 - step is X
    // m_btSide == 1 - step is 0
    uint8_t		m_btSide;
};

/* SZ
struct SGammonOStep
{
	uint32_t	m_nTableID;
	uint8_t		m_arnStep[10];
};
*/

/*
	Структура исходящего короткого сообщения типа КОМАНДА <TableID> <Параметр>
*/
struct SGammonMsg
{
	char		m_chCmd;
	uint32_t	m_nTableID;
	char		m_chData;
};

/*
	Структура исходящего длинного сообщения типа КОМАНДА <TableID> <Параметр> <Данные>
*/

struct SGammonBigMsg : public SGammonMsg
{
	char        m_nPlayerNbr; 
	char		m_arData[30];
};

struct SGammonCubeMsg : public SGammonMsg
{
	char m_chCube1;
	char m_chCube2;	
};


#endif
