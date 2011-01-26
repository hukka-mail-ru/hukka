#include "queuemsg.h"

#include <iostream>

CQueueMsg::CQueueMsg():
	 m_isGetEmpty( true )
{
    pthread_mutex_init(&this->m_mutQueue, NULL);
}

CQueueMsg::~CQueueMsg()
{

}

bool CQueueMsg::AddMsg( const ClientMsg& _clientMsg )
{
	pthread_mutex_lock( &m_mutQueue );
	bool isRes = m_isGetEmpty;
/*
	if( isRes )
		std::cout << "CQueueMsg::AddMsg-Empty" << std::endl;
	else
		std::cout << "CQueueMsg::AddMsg-No Empty"  << std::endl;
*/
	m_queueMsg.push( _clientMsg );
	m_isGetEmpty = false;
	pthread_mutex_unlock( &m_mutQueue );

	return isRes;
}

bool CQueueMsg::GetMsg( ClientMsg& _clientMsg )
{
	bool isRes = false;

	pthread_mutex_lock( &m_mutQueue );
	if( isRes = !m_queueMsg.empty() )
	{
//		std::cout << "CQueueMsg::GetMsg-No Empty" << std::endl;
		_clientMsg = m_queueMsg.front();
		m_queueMsg.pop();
	}
	else
	{
//		std::cout << "CQueueMsg::GetMsg-Empty" << std::endl;
		m_isGetEmpty = true;
	}
	pthread_mutex_unlock( &m_mutQueue );

	return isRes;
}


