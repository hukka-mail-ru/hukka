
#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <syslog.h>

#include "../libs/server/selector.h"
#include "../libs/server/listener.h"

#define GMS_DEBUG_READ

int main(int argc, char *argv[])
{
	openlog( "socketserver", LOG_CONS, LOG_LOCAL0 );

	Listener Listener;
	Listener.Listen( 1234 );

	Selector::Instance()->StartLoop();

	Listener.Close();

	Selector::KillObject();

	return 0;
}
