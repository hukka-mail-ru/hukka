#include "tester.h"

#include <iostream>
#include <sstream>

CTester::CTester() : m_bisContinue( true )
{
	std::stringstream ss;
	ss << ( int ) m_nCount;
	m_strLogin = "tester" + ss.str();
	m_strPassword = "tester" + ss.str();
	
}

int CTester::Run()
{
	usleep( 1+(int) (100000.0*rand()/(RAND_MAX+1.0)) );
	
	if ( !testConnect() )
		return 0;
	
	if ( !testAuthorize() )
		return 0;
	
	while ( m_bisContinue )
	{
		usleep( 1+(int) (100000.0*rand()/(RAND_MAX+1.0)) );
		
		if ( !testSendingCmd() )
			break;
	}
	
}

bool CTester::sendMsg( stlstr *_pstrMsg )
{
    SOutMsg sMsg;
    int     nTo;
    stlstr  strMsg;
    char    strData[ 128 ];

    int nRes = sscanf( _pstrMsg->c_str(), "%d-%d-%s", &nTo, &sMsg.m_chCmd, strData );

    if ( nRes != 3 )
    	return false;

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
                     
    m_Socket.sendMsg( nTo, &sMsg, n );
    
    delete []sMsg.m_arData;
    
    return true;
	
}

bool CTester::testConnect()
{
    if ( ! m_Socket.ConnectToHost( "wapserver3.wapportal.ru", 1234 ) )
    {
      std::cout << m_strLogin << " connection error!" << std::endl;
      return false;
    }
    
    return false;	
}

bool CTester::testAuthorize()
{
	stlstr str( "1-1-"+m_strLogin+"_"+m_strPassword );
	
	if ( !sendMsg( &str ) )
	{
		std::cout << m_strLogin << " sending authorization error" << std::endl;
		return false;
	}
	
}

bool CTester::testSendingCmd()
{
	
}

CTester::~CTester()
{
}
