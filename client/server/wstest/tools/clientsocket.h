/* 
 * File:   clientsocket.h
 * Author: leha
 *
 * Created on 7 Май 2008 г., 14:03
 * 3292864
 */

#ifndef _CLIENTSOCKET_H
#define	_CLIENTSOCKET_H 

#include <string>

#include "socket.h"
#include "mythread.h"

typedef std::string stlstr;

class CClientSocket : public CSocket, public CMyThread
{
public:
    CClientSocket();
    virtual ~CClientSocket();
    
    bool            ConnectToIP( stlstr _strHostIP, int _nPort );
    bool            ConnectToHost( stlstr _strHostName, int _nPort );
    void            stopsocket() { m_bRunning = false; }
    
protected:
    virtual void    connected() {};
    void            disconnected() {};
    void            newdata();
    
private:
    int             Run();
    bool            m_bRunning;

};


#endif	/* _CLIENTSOCKET_H */

