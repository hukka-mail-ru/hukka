#ifndef _SRVSERVER_H
#define _SRVSERVER_H

#include <netinet/in.h>

#include "socketmanager.h"
#include "../socket/clientsocket.h"
#include "onlinemanager.h"
#include "accessmanager.h"

class CSRVServer : public CSocketManager
{
public:

	static CSRVServer*		Instance();
	static void			FreeInst();
	static void			KillObject();

	void					AddSocket( int, const sockaddr_in* );

	void					OnClose( CMySocket* );
private:

	CSRVServer();
	~CSRVServer();

	void					DoAllMsg( CMySocket* );

	void					DoAllMsg( CClientSocket* );
	bool					UnRegister( const CClientMsg*, CClientMsg*, CRegInfo*, ISender* );
	bool					Register( const CClientMsg*, CClientMsg*, CRegInfo* );
private:

	static CSRVServer*		m_pSelf;
	static int			m_nRefCount;

	CAccessManager			m_accessManager;

	COnLineManager*		m_pOnLineManager;
};

#endif
