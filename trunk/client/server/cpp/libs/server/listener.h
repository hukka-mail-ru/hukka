#ifndef _LISTENER_H
#define _LISTENER_H

#include "interface.h"
#include <sys/types.h>
#include "srvserver.h"

class CListener : public ICallBack
{
public:

	CListener();
	virtual ~CListener();

	int			Listen( uint16_t );
	int			Close();

	int			GetSocket() const;
private:

	void			DoRead();
	void			DoWrite();
private:

	int			m_nSocket;

	CSRVServer*	m_pSRVServer;
};

#endif
