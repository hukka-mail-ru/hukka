/*
 * File:   wstest.cpp
 * Author: leha
 *
 * Created on 7 Май 2008 г., 14:06
 */

#include <stdlib.h>
#include <string.h>
#include <fstream>
#include <iostream>
#include <deque>

#include "mysocket.h"
#include "service.h"
/*
 *
 */

CMySocket 	m_Socket;


std::deque<stlstr> m_arCommands;

#define WSTEST_INPUT_FILE "wstest_input.txt"

bool readFile()
{
    std::ifstream conf_file( WSTEST_INPUT_FILE );

    if ( !conf_file )
    {
        std::cout << "could not read /opt/wstest_input.txt" << std::endl;
        std::cout << "commands list not updated" << std::endl;
        return false;
    }

    stlstr strCmd;

    while ( getline( conf_file, strCmd) )
        m_arCommands.push_back( strCmd );

    m_arCommands.pop_back();

    system( "rm "WSTEST_INPUT_FILE);

    return true;
}

void sigfunc( int _nParam )
{

    m_arCommands.clear();

    if ( ! readFile() )
    	return;

    m_Socket.sendMsgFromList();

}

bool sendMsg( CMySocket* _pSocket, stlstr* _pstrMsg )
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

    _pSocket->sendMsg( nTo, &sMsg, n );

    delete []sMsg.m_arData;

    return true;

}

void loginMenu(stlstr* _strLogin, stlstr* _strPassword)
{
	std::cout << "login: ";
    std::cin >> *_strLogin;
    std::cout << std::endl;
    std::cout << "password: ";
    std::cin >> *_strPassword;
    std::cout << std::endl;
}

void showMenu()
{
	std::cout << "Main menu" << std::endl;
	std::cout << "---------" << std::endl;
	std::cout << "[R]egistration" << std::endl;
	std::cout << "[A]uthorization" << std::endl;
	std::cout << "Send message with data like string" << std::endl;
	std::cout << "Send message with data like [i]nt" << std::endl;
	std::cout << "Send message with data like arra[y] of ints" << std::endl;
	std::cout << "Send message with data like [T]O-CMD-<uint32_t>-<string>" << std::endl;
	std::cout << "Send [p]ack of messages" << std::endl;
	std::cout << "[Q]uit" << std::endl;
}

int main(int argc, char** argv)
{

//    if ( ! m_Socket.ConnectToHost( "192.168.56.101", 1234 ) )
    if ( ! m_Socket.ConnectToHost( "localhost", 1234 ) )
    {
      std::cout << "Connection error!" << std::endl;
      return (EXIT_SUCCESS);
    }

    m_Socket.setCmdList( &m_arCommands );
	m_Socket.setAutoMode( true );

    stlstr strMsg, strLogin, strPassword;

    signal( SIGHUP, sigfunc );

    std::cout << "'h' - help" << std::endl;

    while ( strMsg != "q" )
    {
		strMsg = "";

        std::cin >> strMsg;

		if ( strMsg == "q")
	        {
		    	m_Socket.stopsocket();
	            sleep( 1 );
	            break;
	        }

		if ( strMsg == "h")
		{
			showMenu();
		}
		else if ( strMsg == "r" )
	    {
	    	loginMenu(&strLogin, &strPassword);
	      	strMsg = "2-1-" + strLogin + "_" + strPassword;
	        if ( ! sendMsg( &m_Socket, &strMsg ) )
    	    	std::cout << "Error" << std::endl;
	    }
        else if ( strMsg == "a" )
        {
        	loginMenu(&strLogin, &strPassword);
        	strMsg = "1-1-" + strLogin + "_" + strPassword;
            if ( ! sendMsg( &m_Socket, &strMsg ) )
        		std::cout << "Error" << std::endl;
        }
        else if ( strMsg == "c" )
	    {
	    	std::cout << "send message TO-CMD-DATA: ";
    		std::cin >> strMsg;
    		m_arCommands.push_back(strMsg);
		    m_Socket.sendMsgFromList();
	    }
	    else if ( strMsg == "t" )
	    {
	    	std::cout << "send message [T]O-CMD-<uint32_t>-<string>: ";
    		std::cin >> strMsg;
    		m_arCommands.push_back(strMsg);
		    m_Socket.sendMsgFromListExt();
	    }
		else if ( strMsg == "s")
		{
		    sleep(1);
		}
        else if ( strMsg == "i" )
        {
        	std::cout << "send message TO-CMD-<uint32_t>: ";
        	std::cin >> strMsg;
        	std::cout << std::endl;
    		m_arCommands.push_back(strMsg);
		    m_Socket.sendMsgFromListInt32();
        }
        else if ( strMsg == "p" )
		{
	    	int nCount = 0;
	    	std::cout << "count of message: ";
	    	std::cin >> nCount;
	    	for(int i = 0; i < nCount; ++i)
	    	{
	    		std::cout << "message #" << i << " TO-CMD-DATA: ";
    			std::cin >> strMsg;
    			m_arCommands.push_back(strMsg);
	    	}

	    	while (m_arCommands.size())
		    	m_Socket.sendMsgFromList();
		}
		else if ( strMsg == "y" )
		{
			std::cout << "send message TO-CMD: ";
			std::cin >> strMsg;
			m_arCommands.push_back(strMsg);
			std::vector<uint32_t> nvecPrms;

			int nCount = 0;
			std::cout << "count of parameters: ";
			std::cin >> nCount;
			uint32_t nParam;
			for(int i = 0; i < nCount; ++i)
			{
				std::cout << "param #" << i << ": ";
				std::cin >> nParam;
				nvecPrms.push_back(nParam);
			}
			m_Socket.sendMsgIntPrms( nvecPrms );
		}
        else if (strMsg.size())
        {
	       	std::cout << "Syntax error" << std::endl;
	       	break;
		//continue;
        }
		else
		{
	    	while(true)
				sleep(100);
		}


    }

    m_Socket.sockclose();

    return (EXIT_SUCCESS);
}

