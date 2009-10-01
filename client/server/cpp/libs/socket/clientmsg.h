#ifndef CLIENTMSG_H
#define CLIENTMSG_H

#include <sys/types.h>
#include "../tools/buffer.h"
#include "../tools/structs.h"

#pragma pack(1)
struct SHeadMsg
{
	char			m_cSign;
	uint32_t		m_nSize;
};
struct SExHeadMsg : public SHeadMsg
{
	char			m_cVersion;
	uint32_t		m_nFromTo;
};
struct SCommandMsg : public SExHeadMsg
{
	char			m_nCommand;
}; 
struct SMsgError : public SCommandMsg 
{
	char			m_nError;
	char			m_nCRC;
};
#pragma pack()

class CClientMsg
{
public:

	enum ETypeData { etpExHead, etpCommand };
public:

	CClientMsg();
	virtual ~CClientMsg();

	void				InitError( uint32_t, char, char );
	void				InitMsg( uint32_t, TVecChar );
	void				InitMsg( uint32_t, char, TVecChar );

	bool				ParseData( CBuffer*, int& );

	uint32_t			GetTo() const;
	char				GetCommand() const;
	TVecChar*			GetData( ETypeData, TVecChar* ) const;

	const TVecChar*	GetBuffMsg() const;
private:
	
	TVecChar			m_vecDataMsg;
};

#endif
