#include "queuemsg.h"

#include <iostream>

QueueMsg::QueueMsg():
	 m_isGetEmpty( true )
{
    pthread_mutex_init(&this->m_mutQueue, NULL);
}

QueueMsg::~QueueMsg()
{

}

bool QueueMsg::AddMsg( const ClientMsg& _clientMsg )
{
	pthread_mutex_lock( &m_mutQueue );
	bool isRes = m_isGetEmpty;
/*
	if( isRes )
		std::cout << "QueueMsg::AddMsg-Empty" << std::endl;
	else
		std::cout << "QueueMsg::AddMsg-No Empty"  << std::endl;
*/
	m_queueMsg.push( _clientMsg );
	m_isGetEmpty = false;
	pthread_mutex_unlock( &m_mutQueue );

	return isRes;
}

bool QueueMsg::GetMsg( ClientMsg& _clientMsg )
{
	bool isRes = false;

	pthread_mutex_lock( &m_mutQueue );
	if( isRes = !m_queueMsg.empty() )
	{
//		std::cout << "QueueMsg::GetMsg-No Empty" << std::endl;
		_clientMsg = m_queueMsg.front();
		m_queueMsg.pop();
	}
	else
	{
//		std::cout << "QueueMsg::GetMsg-Empty" << std::endl;
		m_isGetEmpty = true;
	}
	pthread_mutex_unlock( &m_mutQueue );

	return isRes;
}


