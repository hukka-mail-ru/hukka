#ifndef _CHTSERVER_H
#define _CHTSERVER_H

#include <stdint.h>

#include "../libs/sql/sqltable.h"
#include "../libs/tools/sqllogiclist.h"
#include "../socket/sendedmsg.h"
#include "accessinfo.h"
#include "socketmanager.h"

class CCHTServer : public SocketManager, public AccessInfo
{
public:

	static CCHTServer*		Instance();
	static void			FreeInst();
	static void			KillObject();
private:

	CCHTServer();
	~CCHTServer();

	void				DoAllMsg( MySocket* );
    
    bool                checkParticipation( uint32_t _nPlayerID, uint32_t _nLogicID, uint32_t _nTableID = 0 );
    
    bool                getGameOnlineUsersTable( uint32_t _nLogicID, CSqlTable* _pRes );
    bool                getLogicChatTable( uint32_t _nLogicID, CSqlTable* _pRes );
    bool                getBoardChatTable( uint32_t _nLogicID, CSqlTable* _pRes );
    
    void                newMsg( ClientMsg* _pMsg );
        
    void                joinToChat( uint32_t _nPlayerID, uint32_t _nLogicID, uint32_t _nTableID = 0 );
    void                leaveChat( uint32_t _nPlayerID, uint32_t _nLogicID, uint32_t _nTableID = 0 );
    void                messageToChat( uint32_t _nPlayerID, uint32_t _nLogicID, 
                                       const TVecChar* _vecData, uint32_t _nTableID = 0 );
    void                sendMsgToChat( uint32_t _nLogicID, CMyStr* _strMsg, uint32_t _nTableID = 0 );
    void                sendMsg( uint32_t _nTo, CSendedMsg *_pMsg );
    void                setSocket( MySocket * _pSocket );
private:

	static CCHTServer*		m_pSelf;
	static int			m_nRefCount;
    MySocket*          m_pSocket;
    CSqlLogicList       m_sqlLogicList;
};

#endif
