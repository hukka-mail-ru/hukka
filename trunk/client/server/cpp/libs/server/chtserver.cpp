#include "chtserver.h"
#include "../header/defserver.h"

CCHTServer* CCHTServer::m_pSelf = 0;
int CCHTServer::m_nRefCount = 0;

CCHTServer* CCHTServer::Instance()
{
	if( m_pSelf == 0 )
		m_pSelf = new CCHTServer;
	
	++m_nRefCount;
	
	return m_pSelf;
}

void CCHTServer::FreeInst()
{
	--m_nRefCount;
	
	if( m_nRefCount > 0 )
		return;
	
	KillObject();
}

void CCHTServer::KillObject()
{
	if( m_pSelf != 0 )
		delete m_pSelf;
	
	m_pSelf = 0;
	m_nRefCount = 0;
}

CCHTServer::CCHTServer()
	:CAccessInfo( "CHT", "CHT" )
{

}

CCHTServer::~CCHTServer()
{

}

void CCHTServer::DoAllMsg( CMySocket* _pSocket )
{
	CClientMsg inMsg, outMsg;

	while( _pSocket->GetMsg( inMsg ) )
	{
		switch( inMsg.GetTo() )
		{
		case SRV:	break;
		default:	_pSocket->AddMsg( inMsg );
		};
	}
}
