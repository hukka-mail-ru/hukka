
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <syslog.h>

#include "ChessService.h"
#include "../libs/gameserver/CGameServer.h"

using namespace std;

int main(int argc, char *argv[])
{	
	CChessService GammonService;

	CGameServer<CChessService> GameServer;
	
	int res = GameServer.start(&GammonService); 
	
	return res;
}

