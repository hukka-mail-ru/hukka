#ifndef _CONNECTOR_H
#define _CONNECTOR_H

#include <sys/types.h>	//FreeBSD 4.11
#include <netinet/in.h>
#include "../socket/mysocket.h"
#include "../server/accessinfo.h"

class Connector : public MySocket
{
public:

	Connector( ISocketManager* _pISocketManager );
	~Connector();

	bool			Connect( in_port_t, const char*, const AccessInfo* );
private:

	bool			Register( const AccessInfo* );
};

#endif
