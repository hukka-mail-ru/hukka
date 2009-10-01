/* 
 * File:   socket.h
 * Author: leha
 *
 * Created on 7 Май 2008 г., 13:17
 */

#ifndef _SOCKET_H
#define	_SOCKET_H

#include <arpa/inet.h>


#include "buffer.h"

class CSocket 
{
public:
    
    CSocket();
    CSocket( int _nHandle );
    virtual ~CSocket();
    
    int         GetHandle() const;
    int         Send( const void* _pBuf, int _nSize );
    int         Recv( void* _pBuf, int _nSize );
    int         GetData( void* _pBuf, int _nSize );
    void        sockclose();
    
protected:
    void            wait();
    virtual void    disconnected() {};
    virtual void    newdata() {};
    char*        sock_ntop( const struct sockaddr *_sa, socklen_t _salen );
    void         sockclose( int _nHandle );
    int          getNewHandle();
    
    int          m_nHandle;    
    
private:
    void    checkdata();
    
    CBuffer m_Buffer;
};


#endif	/* _SOCKET_H */

