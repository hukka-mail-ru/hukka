#ifndef CGAMESERVER_H_
#define CGAMESERVER_H_

#include "../server/connector.h"
#include "../server/accessinfo.h"
#include "../server/selector.h"

template<class TGameService> class GameServer
{
	
public:

	GameServer() {};
		
	virtual ~GameServer() {};
	
	int start(TGameService* _pGameService)
	{
		Connector Connector( static_cast<ISocketManager*>( _pGameService ) );

		if( !Connector.Connect( 1234, "localhost", static_cast<AccessInfo*>( _pGameService ) ) )
		{
			std::cerr << "server not found" << std::endl;
			return -1;
		}
		
		//if( !Connector.Connect( 1234, "82.146.42.150", static_cast<AccessInfo*>( pGammonService ) ) )
		//	return -1;

		Selector::Instance()->StartLoop();

		Connector.Close( SHUT_RDWR );

		Selector::KillObject();

		return 0;
	}

};

#endif /*CGAMESERVER_H_*/
