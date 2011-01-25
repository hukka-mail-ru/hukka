#include "mysyslog.h"

#include <stdarg.h>
#include <stdio.h>

CMySysLog* CMySysLog::m_pSelf = 0;
int CMySysLog::m_nRefCount = 0;

CMySysLog* CMySysLog::Instance()
{
	if( m_pSelf == 0 )
		m_pSelf = new CMySysLog;
	
	++m_nRefCount;
	
	return m_pSelf;
}

void CMySysLog::FreeInst()
{
	--m_nRefCount;
	
	if( m_nRefCount > 0 )
		return;
	
	KillObject();
}

void CMySysLog::KillObject()
{
	if( m_pSelf != 0 )
		delete m_pSelf;
	
	m_pSelf = 0;
	m_nRefCount = 0;
}


CMySysLog::CMySysLog()
{
	openlog( "wapserver", 0, LOG_LOCAL0 );
}

CMySysLog::~CMySysLog()
{
	closelog();
}

void CMySysLog::LogMessage( int _nPriority, const char* _pcMessage, ... )
{
	va_list arg_list;
    va_start( arg_list, _pcMessage );
    va_end( arg_list );

	vsyslog( _nPriority, _pcMessage, arg_list );
}
