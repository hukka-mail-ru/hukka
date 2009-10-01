#include <sys/types.h>	//FreeBSD 4.11
#include "srvserver.h"
#include "../header/defserver.h"
#include "../header/deferror.h"

CSRVServer* CSRVServer::m_pSelf = 0;
int CSRVServer::m_nRefCount = 0;

CSRVServer* CSRVServer::Instance()
{
	if( m_pSelf == 0 )
		m_pSelf = new CSRVServer;
	
	++m_nRefCount;
	
	return m_pSelf;
}

void CSRVServer::FreeInst()
{
	--m_nRefCount;
	
	if( m_nRefCount > 0 )
		return;
	
	KillObject();
}

void CSRVServer::KillObject()
{
	if( m_pSelf != 0 )
		delete m_pSelf;
	
	m_pSelf = 0;
	m_nRefCount = 0;
}

void CSRVServer::AddSocket( int _nSocket, const sockaddr_in* _pAddr )
{
	CClientSocket* pClientSocket = new CClientSocket( _nSocket, static_cast<ISocketManager*>( this ) );

	m_pSelector->AddHandle( _nSocket, EVFILT_READ, EV_ADD, static_cast<ICallBack*>( pClientSocket ) );
}

void CSRVServer::OnClose( CMySocket* _pSocket )
{
	CClientSocket* pClientSocket = reinterpret_cast<CClientSocket*>( _pSocket );

	if( pClientSocket )
		if( int32_t nID = pClientSocket->GetID() )
			m_pOnLineManager->OffLine( nID );

	CSocketManager::OnClose( _pSocket );
}

CSRVServer::CSRVServer()
{
	m_pOnLineManager = COnLineManager::Instance();
}

CSRVServer::~CSRVServer()
{
	COnLineManager::FreeInst();
}

void CSRVServer::DoAllMsg( CMySocket* _pSocket )
{
	CClientSocket* pClientSocket = reinterpret_cast<CClientSocket*>( _pSocket );

	DoAllMsg( pClientSocket );
}

void CSRVServer::DoAllMsg( CClientSocket* _pSocket )
{
	CClientMsg inMsg, outMsg;

	while( _pSocket->GetMsg( inMsg ) )
	{
		bool isOutMsg;

       std::cerr << "CSRVServer::DoAllMsg TO: " << (int)_pSocket->GetID() << std::endl;

		if( _pSocket->GetID() == 0 )
			isOutMsg = UnRegister( &inMsg, &outMsg, static_cast<CRegInfo*>( _pSocket ), static_cast<ISender*>( _pSocket ) );
		else
			isOutMsg = Register( &inMsg, &outMsg, static_cast<CRegInfo*>( _pSocket ) );

		if( isOutMsg )
			_pSocket->AddMsg( outMsg );
	}
}

bool CSRVServer::UnRegister( const CClientMsg* _pinMsg, CClientMsg* _poutMsg, CRegInfo* _pRegInfo, ISender* _pSender )
{
	uint32_t nID;
	TVecChar vecData;
	char nErr = ERRUNDEF;
	_pinMsg->GetData( CClientMsg::etpCommand, &vecData );

	switch ( _pinMsg->GetTo() )
	{
	case SRV:	nErr = m_accessManager.GetAccessID( _pinMsg->GetCommand(), vecData, nID );	break;
	case REG:	nErr = m_accessManager.RegAccessID( _pinMsg->GetCommand(), vecData, nID );	break;
	default:	nErr = ERRNOACCESSTO;
	}

#ifdef DEBUG // add SZ
       std::cerr << "TO: " << _pinMsg->GetTo() 
                 << " CMD: " << _pinMsg->GetCommand() 
                 << " ID: " << nID 
                 << std::endl << "data [ ";
       for(TVecChar::const_iterator it = vecData.begin(); it != vecData.end(); ++it)
              std::cerr << (int)*it << " ";

       std::cerr << " ]" << std::endl;

#endif


	if( nErr == NOERR )
		if( ( nErr = m_pOnLineManager->OnLine( nID, _pSender ) ) == NOERR )
			_pRegInfo->SetID( nID );

	_poutMsg->InitError( _pinMsg->GetTo(), _pinMsg->GetCommand(), nErr );

	return true;
}

bool CSRVServer::Register( const CClientMsg* _pinMsg, CClientMsg* _poutMsg, CRegInfo* _pRegInfo )
{
	bool isRes = false;
	ISender* pSender = 0;
	uint32_t nTo = _pinMsg->GetTo();

	std::cout << "SRVServer::Register()" << std::endl;
	
	switch( nTo )
	{
	case SRV:	
			break;
	default:
		{
			pSender = m_pOnLineManager->IsOnLine( nTo );
			
			std::cout << "SRVServer::Register() nTo = " << nTo << " pSender = " << pSender << std::endl;
			
			if( isRes = ( pSender == 0 ) )
			{
				std::cout << "err addr" << std::endl;
				_poutMsg->InitError( SRV, 0, ERRNOADDR );
			}
			else
			{
				std::cout << "no err addr" << std::endl;
				TVecChar vecData;
				CClientMsg clientMsg;
				_pinMsg->GetData( CClientMsg::etpExHead, &vecData );
				clientMsg.InitMsg( _pRegInfo->GetID(), vecData );
				std::cout << "SRVServer::Register() - 1" << std::endl;
				pSender->AddMsg( clientMsg );
				std::cout << "SRVServer::Register() - 2" << std::endl;
			}
		}
	}

	std::cout << "!SRVServer::Register()" << std::endl;
	
	return isRes;
}
