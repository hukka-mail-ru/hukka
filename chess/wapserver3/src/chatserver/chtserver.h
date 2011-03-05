#ifndef _CHTSERVER_H
#define _CHTSERVER_H

#include <stdint.h>

#include "../libs/sql/sqltable.h"
#include "../libs/tools/sqllogiclist.h"
#include "../socket/sendedmsg.h"
#include "accessinfo.h"
#include "socketmanager.h"
#include "chatdefs.h"

class CHTServer : public SocketManager, public AccessInfo
{
public:

	static CHTServer*		Instance();
	static void			FreeInst();
	static void			KillObject();
private:

	CHTServer();
	~CHTServer();

	void				DoAllMsg( MySocket* );

    bool                checkParticipation( uint32_t playerID, uint32_t logicID, uint32_t tableID = COMMON_CHAT_ID );

    bool                getGameOnlineUsersTable( uint32_t logicID, SqlTable* _pRes );
    bool                getChatTable( uint32_t logicID, SqlTable* _pRes );

    void                newMsg( ClientMsg* _pMsg );

    void                joinChat( uint32_t playerID, uint32_t logicID, uint32_t tableID = COMMON_CHAT_ID );
    void                leaveChat( uint32_t playerID, uint32_t logicID, uint32_t tableID = COMMON_CHAT_ID );
    void                messageToAll( uint32_t playerID, uint32_t logicID,
                                       const TVecChar* _vecData, uint32_t tableID = COMMON_CHAT_ID );
    void                sendMsgToAll( int cmd, uint32_t logicID, CMyStr* _strMsg, uint32_t tableID = COMMON_CHAT_ID );
    void                sendMsgToOne( int cmd, uint32_t playerID, uint32_t logicID, CMyStr* _strMsg, uint32_t tableID = COMMON_CHAT_ID);

    void                sendServerNote(int cmd, uint32_t playerID, uint32_t logicID, uint32_t tableID = COMMON_CHAT_ID);
    void                sendUserNames(uint32_t playerID, uint32_t logicID, uint32_t tableID);
    void                sendHistory( uint32_t playerID, uint32_t logicID, uint32_t tableID);
    void                deleteHistory( uint32_t logicID, uint32_t tableID);

    void                sendMsg( uint32_t _nTo, CSendedMsg *_pMsg );
    void                setSocket( MySocket * _pSocket );


	static CHTServer*		m_pSelf;
	static int			m_nRefCount;
    MySocket*          m_pSocket;
    CSqlLogicList       m_sqlLogicList;
};

#endif
