#include <sys/types.h>	//FreeBSD 4.11
#include "srvserver.h"
#include "../header/defserver.h"
#include "../header/deferror.h"

#include <iostream>

using namespace std;

SRVServer* SRVServer::m_pSelf = 0;
int SRVServer::m_nRefCount = 0;

SRVServer* SRVServer::Instance()
{
	if( m_pSelf == 0 )
		m_pSelf = new SRVServer;

	++m_nRefCount;

	return m_pSelf;
}

void SRVServer::FreeInst()
{
	--m_nRefCount;

	if( m_nRefCount > 0 )
		return;

	KillObject();
}

void SRVServer::KillObject()
{
	if( m_pSelf != 0 )
		delete m_pSelf;

	m_pSelf = 0;
	m_nRefCount = 0;
}

void SRVServer::AddSocket( int _nSocket, const sockaddr_in* _pAddr )
{
	ClientSocket* pClientSocket = new ClientSocket( _nSocket, static_cast<ISocketManager*>( this ) );

	cout << "SRVServer::AddSocket AddHandle" << endl;
	//m_pSelector->AddHandle( _nSocket, EVFILT_READ, EV_ADD, static_cast<ICallBack*>( pClientSocket ) );
	m_pSelector->AddHandle( _nSocket, EPOLLIN, static_cast<ICallBack*>( pClientSocket ) );
}

void SRVServer::OnClose( MySocket* _pSocket )
{
	ClientSocket* pClientSocket = reinterpret_cast<ClientSocket*>( _pSocket );

	if( pClientSocket )
		if( int32_t nID = pClientSocket->GetID() )
			m_pOnLineManager->OffLine( nID );

	SocketManager::OnClose( _pSocket );
}

SRVServer::SRVServer()
{
	m_pOnLineManager = OnLineManager::Instance();
}

SRVServer::~SRVServer()
{
	OnLineManager::FreeInst();
}

void SRVServer::DoAllMsg( MySocket* _pSocket )
{
	ClientSocket* pClientSocket = reinterpret_cast<ClientSocket*>( _pSocket );

	DoAllMsg( pClientSocket );
}

void SRVServer::DoAllMsg( ClientSocket* _pSocket )
{
	ClientMsg inMsg, outMsg;

	while( _pSocket->GetMsg( inMsg ) )
	{
		bool isOutMsg;

       std::cerr << "SRVServer::DoAllMsg TO: " << (int)_pSocket->GetID() << std::endl;

		if( _pSocket->GetID() == 0 )
			isOutMsg = UnRegister( &inMsg, &outMsg, static_cast<RegInfo*>( _pSocket ), static_cast<ISender*>( _pSocket ) );
		else
			isOutMsg = Register( &inMsg, &outMsg, static_cast<RegInfo*>( _pSocket ) );

		if( isOutMsg )
			_pSocket->AddMsg( outMsg );
	}
}

bool SRVServer::UnRegister( const ClientMsg* _pinMsg, ClientMsg* _poutMsg, RegInfo* _pRegInfo, ISender* _pSender )
{
	uint32_t nID;
	TVecChar vecData;
	char nErr = ERRUNDEF;
	_pinMsg->GetData( ClientMsg::etpCommand, &vecData );

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

bool SRVServer::Register( const ClientMsg* _pinMsg, ClientMsg* _poutMsg, RegInfo* _pRegInfo )
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
				_poutMsg->InitError( SRV, nTo, ERRNOADDR );
			}
			else
			{
				std::cout << "no err addr" << std::endl;
				TVecChar vecData;
				ClientMsg clientMsg;
				_pinMsg->GetData( ClientMsg::etpExHead, &vecData );
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
