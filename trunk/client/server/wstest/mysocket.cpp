
#include <vector>

#include "mysocket.h"

#include <iostream>

#include "tools/buffer.h"
#include "tools/clientmsg.h"

CMySocket::CMySocket() : m_bAutoMode( false ), m_pCommands( 0 )
{
    
}

CMySocket::~CMySocket()
{
    
}

void CMySocket::connected()
{
    std::cout << "Connection done" << std::endl;
}

void CMySocket::disconnected()
{
    std::cout << "Disconnected" << std::endl;
}

void CMySocket::newdata()
{
	
	CBuffer buffer;
    CClientMsg climsg;
    
    buffer.AddDataSize( GetData( buffer.GetDataEnd(), 255 ) );
    
    if ( buffer.DataSize() == -1 )
        return;
    
    int nParseErr;

    while(climsg.ParseData( &buffer, nParseErr ))
    {    
        TVecChar vecMsg;
        
	    climsg.GetData( CClientMsg::etpCommand, &vecMsg );
	    
	    if ( (climsg.GetTo() == 1 || climsg.GetTo() == 2) && ( int ) vecMsg[ 0 ] == 0 )
	    	std::cout << "ok" << std::endl;
	    else
	    {
	    	std::cout << "message from " << ( int ) climsg.GetTo() << " CMD: " << ( int ) climsg.GetCommand() << " ARG: ";
		for ( TVecChar::const_iterator i = vecMsg.begin(); i != vecMsg.end(); ++i )
			std::cout << (int) *i << " ";
		std::cout << std::endl;
	    	//sockclose();
	    }
	
	            
	    if ( m_bAutoMode )
	    	sendMsgFromList();
    }
}

void CMySocket::setCmdList( std::deque<stlstr>* _pCommands )
{
	m_pCommands = _pCommands;
}

void CMySocket::sendMsg( int _nTo, SOutMsg *_pMsg, int _nSize )
{
    TVecChar vecMsg;
    vecMsg.reserve( _nSize + 1 );
    
    CClientMsg Msg;
    char *a = _pMsg->m_arData;
    vecMsg.push_back( _pMsg->m_chCmd );
    
    for( int i = 0; i < _nSize; ++i )
    {
        vecMsg.push_back( *( a++ ) );
    }
        
    Msg.InitMsg( _nTo, vecMsg );
    
    //std::cout << "Output message TO:" << ( int ) Msg.GetTo() << " CMD: " << ( int ) Msg.GetCommand() << " ARG: " << vecMsg[ 1 ] << std::endl;
    
    Send( &(*Msg.GetBuffMsg())[0], Msg.GetBuffMsg()->size() );    
}

void CMySocket::sendMsgExt( int _nTo, SOutMsgExt *_pMsg, int _nSize )
{
    TVecChar vecMsg( sizeof(_pMsg->m_chCmd) + sizeof(_pMsg->m_nTableID) + _nSize );
    
    CClientMsg Msg;
    TVecChar::iterator it = vecMsg.begin(); 
    *it = _pMsg->m_chCmd;
    
    ++it;
    
    std::copy( (char*)&(_pMsg->m_nTableID),  (char*)&(_pMsg->m_nTableID)+ sizeof(_pMsg->m_nTableID), it);
    
    it += sizeof(_pMsg->m_nTableID);
    
    std::copy( _pMsg->m_arData, _pMsg->m_arData + _nSize, it);
    
    Msg.InitMsg( _nTo, vecMsg );

    /*std::cout << "message from " << ( int ) Msg.GetTo() << " CMD: " << ( int ) Msg.GetCommand() << " ARG: ";
       for ( TVecChar::const_iterator i = vecMsg.begin(); i != vecMsg.end(); ++i )
           std::cout << (int) *i << " ";
    std::cout << std::endl;*/
    
    Send( &(*Msg.GetBuffMsg())[0], Msg.GetBuffMsg()->size() );    
}

void CMySocket::sendMsgFromList()
{
	if ( m_pCommands->empty() )
		return;
	
	stlstr strMsg = m_pCommands->front();
	m_pCommands->pop_front();
	
    SOutMsg sMsg;
    int     nTo;
    int     nCmd;
    char    strData[ 128 ];

    int nRes = sscanf( strMsg.c_str(), "%d-%d-%s", &nTo, &nCmd, strData );

    sMsg.m_chCmd = nCmd;
    
    if ( nRes != 3 )
    	sendMsgFromList();

    char* a = strData;
    int n = 0;

    do
    {
        if ( *a == '_')
            *a = 0;
        ++n;
    } while( *(++a) );
    
    a -= n;
    
    sMsg.m_arData = new char[ n ];

    memcpy( sMsg.m_arData, a, n );
                     
    sendMsg( nTo, &sMsg, n );
    
    delete []sMsg.m_arData;
}

void CMySocket::sendMsgFromListExt()
{
	if ( m_pCommands->empty() )
		return;
	
	stlstr strMsg = m_pCommands->front();
	m_pCommands->pop_front();
	
    SOutMsgExt sMsg;
    int     nTo;
    int     nCmd;
    int     nTbl;
    char    strData[ 128 ];

    int nRes = sscanf( strMsg.c_str(), "%d-%d-%d-%s", &nTo, &nCmd, &(sMsg.m_nTableID), strData );

    sMsg.m_chCmd = nCmd;
    
    if ( nRes != 3 )
    	sendMsgFromList();

    char* a = strData;
    int n = 0;

    do
    {
        if ( *a == '_')
            *a = 0;
        ++n;
    } while( *(++a) );
    
    a -= n;
    
    sMsg.m_arData = new char[ n ];

    memcpy( sMsg.m_arData, a, n );
                     
    sendMsgExt( nTo, &sMsg, n );
    
    delete []sMsg.m_arData;
}


void CMySocket::sendMsgFromListInt32()
{
	if ( m_pCommands->empty() )
		return;
	
	stlstr strMsg = m_pCommands->front();
	m_pCommands->pop_front();
	
    SOutMsg sMsg;
    int     nTo;
    int     nCmd;
    
    int size =  sizeof(int);
    sMsg.m_arData = new char[size];
    
    int nID = 0;

    int nRes = sscanf( strMsg.c_str(), "%d-%d-%d", &nTo, &nCmd, sMsg.m_arData);
    
    sMsg.m_chCmd = nCmd;
    
    sendMsg( nTo, &sMsg, size );
    
    delete []sMsg.m_arData;
}

void CMySocket::sendMsgIntPrms( std::vector<uint32_t> &_pvecPrms )
{
	if ( m_pCommands->empty() )
		return;
	
	stlstr strMsg = m_pCommands->front();
	m_pCommands->pop_front();
	
	SOutMsg sMsg;
	int     nTo;
	int     nCmd;
        
	int nID = 0;

	int nRes = sscanf( strMsg.c_str(), "%d-%d", &nTo, &nCmd );
    
	sMsg.m_chCmd = nCmd;
			
	sMsg.m_arData = (char*)&_pvecPrms[0];
	    	
	sendMsg( nTo, &sMsg, _pvecPrms.size()*sizeof(uint32_t) );
    	
}

void CMySocket::setAutoMode( bool _bAutoMode )
{
	m_bAutoMode = _bAutoMode;
}
