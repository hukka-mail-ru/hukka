#ifndef _ACCESSMANAGER_H
#define _ACCESSMANAGER_H

#include "../socket/clientmsg.h"
#include "../sql/sqltableusers.h"

class COnLineManager;

class CAccessManager
{
public:

	CAccessManager();
	~CAccessManager();

//	uint32_t			GetAccessID( uint32_t, CClientMsg&, CClientMsg* );
	char				GetAccessID( char, const TVecChar&, uint32_t& );
	char				RegAccessID( char, const TVecChar&, uint32_t& );
private:

	bool				Parse2str( const TVecChar&, TVecChar*, TVecChar* );
private:

	CSqlTableUsers		m_sqlTableUsers;
	COnLineManager*	m_pOnLineManager;
};

#endif
