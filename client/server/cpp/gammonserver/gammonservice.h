#ifndef GAMMONSERVICE_H
#define GAMMONSERVICE_H

/**
	@author WapPortal.RU <office@wapportal.ru>
*/

#include "gammontable.h"
#include "../libs/socket/clientmsg.h"
#include "../libs/socket/mysocket.h"
#include "../libs/server/accessinfo.h"
#include "../libs/server/socketmanager.h"
#include "gammonstructs.h"

class CGammonService : public CSocketManager, public CAccessInfo
{
public:
	
	static CGammonService* Instance();
	static void FreeInst();
	static void KillObject();
	
private:
	CGammonService();
	~CGammonService();
	
	void	newMsg( CClientMsg *_pClientMsg );
	void	setSocket( CMySocket *_pSocket );
	
	void	cmdJoin( uint32_t _nPlayerID, uint32_t _nTableID );
	void	cmdStep( uint32_t _nPlayerID, SGammonStep _sStep );
	
	CGammonTable		m_Table;
	CMySocket	*m_pSocket;
	
	static CGammonService*		m_pSelf;
	static int			m_nRefCount;


private:
	void cmdGetField( uint32_t _nPlayerID, uint32_t _nTableID );
	void cmdLoose( uint32_t _nPlayerID, uint32_t _nTableID );
	void DoAllMsg( CMySocket * _pSocket );
	void OnClose( CMySocket* );
	

};

#endif
