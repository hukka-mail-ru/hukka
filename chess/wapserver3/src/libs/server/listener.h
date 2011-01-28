#ifndef _LISTENER_H
#define _LISTENER_H

#include "interface.h"
#include <sys/types.h>
#include "srvserver.h"

class Listener : public IReaderWriter
{
public:

	Listener();
	virtual ~Listener();

	int			Listen( uint16_t );
	int			Close();

	int			GetSocket() const;
private:

	void			DoRead();
	void			DoWrite();
private:

	int			m_nSocket;

	SRVServer*	m_pSRVServer;
};

#endif
