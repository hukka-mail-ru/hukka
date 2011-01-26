
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <syslog.h>

#include "ChessService.h"
#include "../libs/gameserver/GameServer.h"

using namespace std;

int main(int argc, char *argv[])
{	
	ChessService GammonService;

	GameServer<ChessService> GameServer;
	
	int res = GameServer.start(&GammonService); 
	
	return res;
}

