#ifndef SERVICE_H_
#define SERVICE_H_

#include <set>
#include <signal.h>
#include <stdlib.h>
#include <string>
#include <signal.h>
#include <sys/stat.h>
#include <unistd.h>

typedef std::set<std::string> strSet;

typedef void Sigfunc( int );

strSet  arCommands;

Sigfunc* signal( int _nSignal, Sigfunc *func )
{
    struct sigaction act, oact;
    
    act.sa_handler = func;
    sigemptyset( &act.sa_mask );
    act.sa_flags = 0;
    
    if ( _nSignal == SIGALRM ) {
#ifdef SA_INTERRUPT
        act.sa_flags |= SA_INTERRUPT;
#endif
    } else {
#ifdef SA_RESTART
        act.sa_flags |= SA_RESTART;
#endif        
    }
    if ( sigaction ( _nSignal, &act, &oact ) < 0 )
        return ( SIG_ERR );
    return ( oact.sa_handler );
}

void daemon_init()
{
    int i;
    pid_t pid;
    
    if ( ( pid = fork() ) != 0 )
        exit( 0 );
    
    setsid();
    
    signal( SIGHUP, SIG_IGN );
    
    if ( ( pid = fork() ) != 0 )
        exit( 0 );
    
    chdir("/");
    umask( 0 );
    
    for ( i = 0; i < 64; ++i )
        close( i );
    
}
#endif /*SERVICE_H_*/
