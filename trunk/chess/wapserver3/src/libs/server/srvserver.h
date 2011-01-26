#ifndef _SRVSERVER_H
#define _SRVSERVER_H

#include <netinet/in.h>

#include "socketmanager.h"
#include "../socket/clientsocket.h"
#include "onlinemanager.h"
#include "accessmanager.h"

class SRVServer : public SocketManager
{
public:

	static SRVServer*		Instance();
	static void			FreeInst();
	static void			KillObject();

	void					AddSocket( int, const sockaddr_in* );

	void					OnClose( MySocket* );
private:

	SRVServer();
	~SRVServer();

	void					DoAllMsg( MySocket* );

	void					DoAllMsg( ClientSocket* );
	bool					UnRegister( const ClientMsg*, ClientMsg*, RegInfo*, ISender* );
	bool					Register( const ClientMsg*, ClientMsg*, RegInfo* );
private:

	static SRVServer*		m_pSelf;
	static int			m_nRefCount;

	AccessManager			m_accessManager;

	OnLineManager*		m_pOnLineManager;
};

#endif
