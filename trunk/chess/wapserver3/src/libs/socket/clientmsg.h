#ifndef CLIENTMSG_H
#define CLIENTMSG_H

#include <sys/types.h>
#include "../tools/buffer.h"
#include "../tools/structs.h"

#pragma pack(1)
struct HeadMsg
{
	char			m_cSign;
	uint32_t		m_nSize;
};
struct ExHeadMsg : public HeadMsg
{
	char			m_cVersion;
	uint32_t		m_nFromTo;
};
struct CommandMsg : public ExHeadMsg
{
	char			m_nCommand;
}; 
struct MsgError : public CommandMsg 
{
	char			m_nError;
	char			m_nCRC;
};
#pragma pack()

class ClientMsg
{
public:

	enum TypeData { etpExHead, etpCommand };
public:

	ClientMsg();
	virtual ~ClientMsg();

	void				InitError( uint32_t, char, char );
	void				InitMsg( uint32_t, TVecChar );
	void				InitMsg( uint32_t, char, TVecChar );

	bool				ParseData( Buffer*, int& );

	uint32_t			GetTo() const;
	char				GetCommand() const;
	TVecChar*			GetData( TypeData, TVecChar* ) const;

	const TVecChar*	GetBuffMsg() const;
private:
	
	TVecChar			m_vecDataMsg;
};

#endif
