#include <errno.h>
#include "../header/deferror.h"
#include "mysocket.h"
#include "clientmsg.h"
#include "../server/socketmanager.h"

#include <iostream>

using namespace std;

pthread_mutex_t	m_mutRW = PTHREAD_MUTEX_INITIALIZER;

MySocket::MySocket( int _nSocket, ISocketManager* _pSocketManager )
	:Socket( _nSocket ),
	 m_pSocketManager( _pSocketManager )
{

}

MySocket::~MySocket()
{

}

void MySocket::AddMsg( const ClientMsg& _clientMsg )
{
#ifdef LOW_LEVEL_DEBUG
    cout << "SOCKET " << m_nSocket << " MySocket::AddMsg. ADD MSG" << endl;
#endif

	pthread_mutex_lock( &m_mutRW );
#ifdef GMS_DEBUG
    std::cerr << "MySocket::AddMsg() to : " << _clientMsg.GetTo() << std::endl;
    std::cerr << "MySocket::AddMsg() m_outQueueMsg.size() = " << m_outQueueMsg.size() << std::endl;
    std::cerr << "MySocket::AddMsg() m_nSocket = " << m_nSocket << std::endl;
	std::cerr << "MySocket::AddMsg() _clientMsg.GetBuffMsg()->size() = " << _clientMsg.GetBuffMsg()->size() << std::endl;
    std::cerr << "MySocket::AddMsg() _clientMsg.GetBuffMsg() = [ ";
    for(int i = 0; i < _clientMsg.GetBuffMsg()->size(); ++i)
          std::cerr << (int)( _clientMsg.GetBuffMsg()->at(i) ) << " ";
    std::cerr << " ] " << std::endl;
#endif

	m_outQueueMsg.AddMsg( _clientMsg );

	if( m_pSocketManager )
		m_pSocketManager->AddOutMsg( this );

	pthread_mutex_unlock( &m_mutRW );
}

bool MySocket::GetMsg( ClientMsg& _clienMsg )
{
	pthread_mutex_lock( &m_mutRW );
	bool res = m_inQueueMsg.GetMsg( _clienMsg );
	pthread_mutex_unlock( &m_mutRW );
	return res;
}

void MySocket::DoRead()
{
	pthread_mutex_lock( &m_mutRW );
#ifdef GMS_DEBUG_READ
	std::cout << "MySocket::DoRead()-Y" << std::endl;
#endif //GMS_DEBUG_READ

	int nErr;
	bool isBuffFull;
	do
	{

#ifdef GMS_DEBUG_READ
	    std::cerr << "MySocket::DoRead() m_Buffer.FreeSize()  = " << m_Buffer.FreeSize() << std::endl;
#endif //GMS_DEBUG_READ

        int nSize = Recv( m_Buffer.GetDataEnd(), m_Buffer.FreeSize() );

#ifdef GMS_DEBUG_READ
        std::cerr << "MySocket::DoRead() nSize = " << nSize << " m_Buffer.FreeSize()  = " << m_Buffer.FreeSize() << std::endl;
	    std::cerr << "MySocket::DoRead()  m_Buffer = [ ";
		for(char* c = m_Buffer.GetDataEnd(); c !=  m_Buffer.GetDataEnd() + nSize; ++c)
		{
			if ( (int) *c > 31)
				std::cerr << *c;
			else
				std::cerr << '?';
		}
		std::cerr << " ]" << std::endl;
#endif //GMS_DEBUG_READ
		nErr = errno;

		if( nSize > 0 )
		{
			m_Buffer.AddDataSize( nSize );

			if( isBuffFull = ( m_Buffer.FreeSize() == 0 ) )
				m_Buffer.IncBuffer();
		}
		else if( ( nSize  == 0 ) || ( ( nSize < 0 ) && ( nErr != EAGAIN ) && ( nErr != EINTR ) ) )
		{
#ifdef GMS_DEBUG_READ
			std::cout << "MySocket::DoRead()-R" << std::endl;
#endif //GMS_DEBUG_READ
			DoClose();
			pthread_mutex_unlock( &m_mutRW );
			return;
		}
	}
	while( isBuffFull || ( nErr == EINTR ) );

	int nError = 0;
	ClientMsg clientMsg;

	while( clientMsg.ParseData( &m_Buffer, nError ) )
	{
#ifdef GMS_DEBUG_READ
    	std::cerr << "MySocket::DoRead() to : " << clientMsg.GetTo() << std::endl;
    	std::cerr << "MySocket::DoRead() m_inQueueMsg.size() = " << m_inQueueMsg.size() << std::endl;
		std::cerr << "MySocket::DoRead() clientMsg.GetBuffMsg()->size() = " << clientMsg.GetBuffMsg()->size() << std::endl;
    	std::cerr << "MySocket::DoRead() clientMsg.GetBuffMsg() = [ ";
    	for(int i = 0; i < clientMsg.GetBuffMsg()->size(); ++i)
        	  std::cerr << (int)( clientMsg.GetBuffMsg()->at(i) ) << " ";
    	std::cerr << " ] " << std::endl;
#endif //GMS_DEBUG_READ
		if( m_inQueueMsg.AddMsg( clientMsg ) && m_pSocketManager )
		{
#ifdef GMS_DEBUG_READ
			std::cout << "MySocket::DoRead()-AddInMsg" << std::endl;
#endif //GMS_DEBUG_READ
			m_pSocketManager->AddInMsg( this );
		}
#ifdef GMS_DEBUG_READ
		else
			std::cout << "MySocket::DoRead()-No AddInMsg" << std::endl;
#endif //GMS_DEBUG_READ
	}

	if( nError != NOERR )
	{
#ifdef GMS_DEBUG_READ
		std::cout << "MySocket::DoRead()-R" << std::endl;

	    std::cerr << "MySocket::DoRead() nError = " << nError << std::endl;
	    std::cerr << "MySocket::DoRead() Close( SHUT_RD ) - beg" << std::endl;
#endif //GMS_DEBUG_READ
		Close( SHUT_RD );
#ifdef GMS_DEBUG_READ
	    std::cerr << "MySocket::DoRead() Close( SHUT_RD ) - end" << std::endl;
#endif //GMS_DEBUG_READ
		clientMsg.InitError( 1, ERRUNDEF, nError );
		m_outQueueMsg.AddMsg( clientMsg );
		if( m_pSocketManager )
			m_pSocketManager->AddOutMsg( this );
	}
#ifdef GMS_DEBUG_READ
	else
		std::cout << "MySocket::DoRead()-G" << std::endl;
#endif //GMS_DEBUG_READ

	pthread_mutex_unlock( &m_mutRW );
}

void MySocket::DoWrite()
{
	pthread_mutex_lock( &m_mutRW );

	ClientMsg m_clientMsg;
	while( m_outQueueMsg.GetMsg( m_clientMsg ) )
	{
		const TVecChar* pVecMsg = m_clientMsg.GetBuffMsg();

#ifdef GMS_DEBUG
    std::cerr << "+++ MySocket::DoWrite() to : " << m_clientMsg.GetTo() << std::endl;
    std::cerr << "MySocket::DoWrite() m_outQueueMsg.size() = " << m_outQueueMsg.size() << std::endl;
    std::cerr << "MySocket::DoWrite() m_nSocket = " << m_nSocket << std::endl;
	std::cerr << "MySocket::DoWrite() pVecMsg->size() = " << pVecMsg->size() << std::endl;
    std::cerr << "MySocket::DoWrite()  pVecMsg = [ ";
    for(int i = 0; i < pVecMsg->size(); ++i)
          std::cerr << (int)( pVecMsg->at(i) ) << " ";
    std::cerr << " ] " << std::endl;
#endif

		Send( &(*pVecMsg)[0], pVecMsg->size() );
	}
	pthread_mutex_unlock( &m_mutRW );
}

void MySocket::DoClose()
{
	if( m_pSocketManager )
		m_pSocketManager->RemoveSocket( this );

	Close( SHUT_RDWR );
}
