#ifndef _CONNECTOR_H
#define _CONNECTOR_H

#include <sys/types.h>	//FreeBSD 4.11
#include <netinet/in.h>
#include "../socket/mysocket.h"
#include "../server/accessinfo.h"

class CConnector : public CMySocket
{
public:

	CConnector( ISocketManager* _pISocketManager );
	~CConnector();

	bool			Connect( in_port_t, const char*, const CAccessInfo* );
private:

	bool			Register( const CAccessInfo* );
};

#endif
