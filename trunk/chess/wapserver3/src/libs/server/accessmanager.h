#ifndef _ACCESSMANAGER_H
#define _ACCESSMANAGER_H

#include "../socket/clientmsg.h"
#include "../sql/sqltableusers.h"

class OnLineManager;

class AccessManager
{
public:

	AccessManager();
	~AccessManager();

//	uint32_t			GetAccessID( uint32_t, ClientMsg&, ClientMsg* );
	char				GetAccessID( char, const TVecChar&, uint32_t& );
	char				RegAccessID( char, const TVecChar&, uint32_t& );
private:

	bool				Parse2str( const TVecChar&, TVecChar*, TVecChar* );
private:

	SqlTableUsers		m_sqlTableUsers;
	OnLineManager*	m_pOnLineManager;
};

#endif
