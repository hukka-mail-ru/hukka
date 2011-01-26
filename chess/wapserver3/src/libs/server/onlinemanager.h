#ifndef _ONLINEMANAGER_H
#define _ONLINEMANAGER_H

#include <map>
#include <sys/types.h>
#include "../header/interface.h"

typedef std::map<int32_t,ISender*> TMapRegSocket;

class OnLineManager
{
public:

	static OnLineManager*	Instance();
	static void		FreeInst();
	static void		KillObject();

	ISender*		IsOnLine( int32_t );
	char			OnLine( int32_t, ISender* );
	void			OffLine( int32_t );
private:

	OnLineManager();
	~OnLineManager();
private:

	static OnLineManager*	m_pSelf;
	static int		m_nRefCount;

	mutable pthread_mutex_t	m_mutMap;
	TMapRegSocket		m_mapRegSocket;
};

#endif
