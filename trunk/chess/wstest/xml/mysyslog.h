#ifndef MYSYSLOG_H_
#define MYSYSLOG_H_

#include <syslog.h>

class CMySysLog
{
public:

	static CMySysLog*	Instance();
	static void			FreeInst();
	static void			KillObject();

	void				LogMessage( int, const char*, ... );
private:

	CMySysLog();
	virtual ~CMySysLog();
private:

	static CMySysLog*	m_pSelf;
	static int			m_nRefCount;
};

#endif /*MYSYSLOG_H_*/
