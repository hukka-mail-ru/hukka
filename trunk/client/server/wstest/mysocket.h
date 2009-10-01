/* 
 * File:   mysocket.h
 * Author: leha
 *
 * Created on 13 Май 2008 г., 13:19
 */

#ifndef _MYSOCKET_H
#define	_MYSOCKET_H

#include <deque>

#include "tools/clientsocket.h"
#include "tools/structs.h"

class CMySocket : public CClientSocket
{
public:
    CMySocket();
    virtual ~CMySocket();
    void    sendMsg( int _nTo, SOutMsg*, int _nSize );
    void    sendMsgExt( int _nTo, SOutMsgExt *_pMsg, int _nSize );
    void	setAutoMode( bool _bAutoMode );
    void	setCmdList( std::deque<stlstr>* _pCommands );
	void	sendMsgFromList();
	void    sendMsgFromListExt();
	void	sendMsgFromListInt32();
	void	sendMsgIntPrms( std::vector<uint32_t> &_pvecPrms );
	
protected:
    void        connected();
    void        disconnected();
    void        newdata();

private:
	
	bool				m_bAutoMode;
	std::deque<stlstr>*	m_pCommands;


};



#endif	/* _MYSOCKET_H */

