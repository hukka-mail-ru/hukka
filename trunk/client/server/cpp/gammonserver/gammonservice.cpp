#include "gammonservice.h"
#include "../libs/header/defserver.h"
#include "../libs/header/deferror.h"
#include "../libs/header/defservice.h"

#include <syslog.h>


CGammonService* CGammonService::m_pSelf = 0;
int CGammonService::m_nRefCount = 0;

CGammonService::CGammonService() : CAccessInfo( "lgm", "*B7F6B22E1EF7058DA6484F4B0584A1E0BD1C120B" )
{
}

void CGammonService::cmdJoin( uint32_t _nPlayerID, uint32_t _nTableID )
{
	
	syslog( LOG_INFO | LOG_LOCAL0, "CGammonService::cmdJoin( %d , %d )", _nPlayerID, _nTableID );
	
	SGammonMsg sCmd;
	sCmd.m_chCmd = ANS_JOIN;
	sCmd.m_nTableID = _nTableID;
	char strData;
	
	TVecChar vecCmd;
	CClientMsg Msg;
			
	if ( ! m_Table.joinToTable( _nPlayerID, _nTableID ) )
	{
		
		//syslog( LOG_INFO | LOG_LOCAL0, "CGammonService::cmdJoin : !m_Table.joinToTable( %d, %d )", _nPlayerID, _nTableID );
		
		sCmd.m_chData = ( char ) P_FAILED;
		
		//syslog( LOG_INFO | LOG_LOCAL0, "CGammonService::cmdJoin : vecCmd.assign" );
		
		vecCmd.assign( (char*)&sCmd, (char*)(&sCmd)+sizeof( sCmd ) );
		
		//syslog( LOG_INFO | LOG_LOCAL0, "CGammonService::cmdJoin : Msg.InitMsg( %d, vecCmd );", _nPlayerID );
		Msg.InitMsg( _nPlayerID, vecCmd );
		
		//syslog( LOG_INFO | LOG_LOCAL0, "CGammonService::cmdJoin : m_pSocket->AddMsg( Msg );" );
		
		m_pSocket->AddMsg( Msg );	
		syslog( LOG_INFO | LOG_LOCAL0, "Send to %d CMD: %d TableID: %d Param: %d",_nPlayerID, (int)(( SGammonMsg* ) &sCmd)->m_chCmd,(( SGammonMsg* ) &sCmd)->m_nTableID, (int)(( SGammonMsg* ) &sCmd)->m_chData );			
	}
	
	//syslog( LOG_INFO | LOG_LOCAL0, "return from CGammonService::cmdJoin" );
}

void CGammonService::cmdStep( uint32_t _nPlayerID, SGammonStep _sStep )
{
	syslog( LOG_INFO | LOG_LOCAL0, "CGammonService::cmdStep SGammonStep.m_nTableID : %d ", _sStep.m_nTableID );
	syslog( LOG_INFO | LOG_LOCAL0, "CGammonService::cmdStep SGammonStep.m_btSide : %d ", _sStep.m_btSide );
	for(int i = 0; i < 8; ++i)
		syslog( LOG_INFO | LOG_LOCAL0, "CGammonService::cmdStep SGammonStep.m_arnStep[%d] : %d ", i, _sStep.m_arnStep[i] );
		
	m_Table.playerStep( _nPlayerID, _sStep );
}

void CGammonService::cmdGetField( uint32_t _nPlayerID, uint32_t _nTableID )
{
	m_Table.getField( _nPlayerID, _nTableID ); 
}

void CGammonService::cmdLoose( uint32_t _nPlayerID, uint32_t _nTableID )
{
	m_Table.loose( _nPlayerID, _nTableID );	
}

void CGammonService::newMsg( CClientMsg *_pClientMsg )
{
	TVecChar vecCmd;
	_pClientMsg->GetData( CClientMsg::etpCommand, &vecCmd );
	
#ifdef MYDEBUG

	syslog( LOG_INFO | LOG_LOCAL0, "input message from:%d CMD:%d PARAM:%d", ( uint32_t )_pClientMsg->GetTo(), ( uint32_t ) _pClientMsg->GetCommand(), ( uint32_t ) vecCmd[0] );
	
#endif
	if ( _pClientMsg->GetTo() == SRV )
	{
		if ( _pClientMsg->GetCommand() == 1 )
			if ( vecCmd[0] != NOERR )
			{
				syslog( LOG_INFO | LOG_LOCAL0, "connection to socketserver failed" );
				/// @todo Закончить приложение
			}
			else
				syslog( LOG_INFO | LOG_LOCAL0, "started" );		
			 
	}
		
	if ( _pClientMsg->GetTo() < 100 )
	{
		//syslog( LOG_INFO | LOG_LOCAL0, "_pClientMsg->GetTo() < 100" );
		return;
	}
	//std::cerr << 
	switch( _pClientMsg->GetCommand() )
	{
		case CMD_JOIN:
		{			
			syslog( LOG_INFO | LOG_LOCAL0, "case CMD_JOIN" );
			
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];
			
			cmdJoin( _pClientMsg->GetTo(), *nTableID );
			
			break;
		}
		case CMD_STEP:
		{		
			syslog( LOG_INFO | LOG_LOCAL0, "case CMD_STEP" );
			
			SGammonStep* psStep = ( SGammonStep* ) &vecCmd[0];
			
#ifdef GMS_DEBUG
            std::cerr << "CGammonService::newMsg: case CMD_STEP: psStep->m_nTableID = " << psStep->m_nTableID << std::endl;
            std::cerr << "CGammonService::newMsg: case CMD_STEP: psStep->m_arnStep = [ ";
            for(int index = 0; index < 8; ++index)
                std::cerr << (int) psStep->m_arnStep[index] << " ";
            std::cerr << "]" << std::endl;
            std::cerr << "CGammonService::newMsg: case CMD_STEP: " << psStep->m_btSide << std::endl;
#endif
			cmdStep( _pClientMsg->GetTo(), *psStep );
			
			break;
		}
		case CMD_GET_FIELD:
		{
			syslog( LOG_INFO | LOG_LOCAL0, "case CMD_GET_FIELD" );
			
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];
			
			cmdGetField( _pClientMsg->GetTo(), *nTableID );
			
			break;			
		}
		case CMD_LOOSE:
		{
			syslog( LOG_INFO | LOG_LOCAL0, "case CMD_LOOSE" );
			
			uint32_t *nTableID = (uint32_t*) &vecCmd[0];
			
			cmdLoose( _pClientMsg->GetTo(), *nTableID );
			
			break;
		}
		default:	
			break;
	}
}

void CGammonService::setSocket( CMySocket *_pSocket )
{
	m_pSocket = _pSocket;	
	m_Table.setSocket( _pSocket );
}

void CGammonService::DoAllMsg( CMySocket * _pSocket )
{
	setSocket( _pSocket );
	CClientMsg inMsg;
	
	//syslog( LOG_INFO | LOG_LOCAL0, "DoAllMsg( CMySocket * _pSocket )" );
	
	while( _pSocket->GetMsg( inMsg ) )
	{
		newMsg( &inMsg );
		//syslog( LOG_INFO | LOG_LOCAL0, "return from newMsg()" );	
	}
}

void CGammonService::OnClose( CMySocket* _pSocket )
{
}

CGammonService* CGammonService::Instance()
{
	if( m_pSelf == 0 )
		m_pSelf = new CGammonService;
	
	++m_nRefCount;
	
	return m_pSelf;
}


void CGammonService::FreeInst()
{
	--m_nRefCount;
	
	if( m_nRefCount > 0 )
		return;
	
	KillObject();
}


void CGammonService::KillObject()
{
	if( m_pSelf != 0 )
		delete m_pSelf;
	
	m_pSelf = 0;
	m_nRefCount = 0;
}


CGammonService::~CGammonService()
{
}
