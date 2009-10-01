#include "onlinemanager.h"
#include "../header/deferror.h"

#include <iostream>

#define MYDEBUG

int COnLineManager::m_nRefCount = 0;
COnLineManager* COnLineManager::m_pSelf = 0;

COnLineManager* COnLineManager::Instance()
{
	if( m_pSelf == 0 )
		m_pSelf = new COnLineManager;
	
	++m_nRefCount;
	
	return m_pSelf;
}

void COnLineManager::FreeInst()
{
	--m_nRefCount;
	
	if( m_nRefCount > 0 )
		return;
	
	KillObject();
}

void COnLineManager::KillObject()
{
	if( m_pSelf != 0 )
		delete m_pSelf;
	
	m_pSelf = 0;
	m_nRefCount = 0;
}

ISender* COnLineManager::IsOnLine( int32_t _nID )
{
	pthread_mutex_lock( &m_mutMap );
	TMapRegSocket::iterator It = m_mapRegSocket.find( _nID );
	ISender* pSender = It != m_mapRegSocket.end() ? It->second : 0; 
	pthread_mutex_unlock( &m_mutMap );

	return pSender;
}

char COnLineManager::OnLine( int32_t _nID, ISender* _pSender )
{
	char nErr = NOERR;

	pthread_mutex_lock( &m_mutMap );
	if( m_mapRegSocket.find( _nID ) == m_mapRegSocket.end() )
		m_mapRegSocket.insert( std::make_pair( _nID, _pSender ) );
	else
		nErr = ERRUSERONLINE;

#ifdef DEBUG // add SZ
//    std::cerr << "ON LINE: << std::endl;
  //  for(TMapRegSocket::const_iterator it = m_mapRegSocket.begin(); ++it)
    //	std::cerr << "ID: << it->first << std::endl;
#endif

	pthread_mutex_unlock( &m_mutMap );

	
#ifdef MYDEBUG //add SZ
	std::cerr << "ON-LINE" << std::endl;
	for (TMapRegSocket::const_iterator it = m_mapRegSocket.begin(); it != m_mapRegSocket.end(); ++it )
		std::cerr << "ID: " << it->first << std::endl; 
		
#endif
	return nErr;
}

void COnLineManager::OffLine( int32_t _nID )
{
	pthread_mutex_lock( &m_mutMap );
	m_mapRegSocket.erase( _nID );
	pthread_mutex_unlock( &m_mutMap );
}

COnLineManager::COnLineManager()
	:m_mutMap( PTHREAD_MUTEX_INITIALIZER )
{

}

COnLineManager::~COnLineManager()
{

}
