#ifndef _CHTSERVER_H
#define _CHTSERVER_H

#include "accessinfo.h"
#include "socketmanager.h"

class CCHTServer : public CSocketManager, public CAccessInfo
{
public:

	static CCHTServer*		Instance();
	static void			FreeInst();
	static void			KillObject();
private:

	CCHTServer();
	~CCHTServer();

	void				DoAllMsg( CMySocket* );
private:

	static CCHTServer*		m_pSelf;
	static int			m_nRefCount;
};

#endif
