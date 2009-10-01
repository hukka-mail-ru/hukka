
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <syslog.h>

#include "../libs/server/selector.h"
#include "../libs/server/listener.h"

int main(int argc, char *argv[])
{
	openlog( "socketserver", LOG_CONS, LOG_LOCAL0 );
	
	CListener Listener;
	Listener.Listen( 1234 );

	CSelector::Instance()->StartLoop();

	Listener.Close();

	CSelector::KillObject();

	return 0;
}
