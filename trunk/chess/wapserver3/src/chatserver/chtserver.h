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

    bool                checkParticipation( uint32_t _nPlayerID, uint32_t _nLogicID, uint32_t _nTableID = COMMON_CHAT_ID );

    bool                getGameOnlineUsersTable( uint32_t _nLogicID, SqlTable* _pRes );
    bool                getChatTable( uint32_t _nLogicID, SqlTable* _pRes );

    void                newMsg( ClientMsg* _pMsg );

    void                joinChat( uint32_t _nPlayerID, uint32_t _nLogicID, uint32_t _nTableID = COMMON_CHAT_ID );
    void                leaveChat( uint32_t _nPlayerID, uint32_t _nLogicID );
    void                messageToAll( uint32_t _nPlayerID, uint32_t _nLogicID,
                                       const TVecChar* _vecData, uint32_t _nTableID = COMMON_CHAT_ID );
    void                sendMsgToAll( uint32_t _nLogicID, CMyStr* _strMsg, uint32_t _nTableID = COMMON_CHAT_ID );
    void                sendMsgToOne( uint32_t _nPlayerID, uint32_t _nLogicID, CMyStr* _strMsg, uint32_t _nTableID = COMMON_CHAT_ID);

    void                sendMsg( uint32_t _nTo, CSendedMsg *_pMsg );
    void                setSocket( MySocket * _pSocket );

    void                getHistory( uint32_t _nPlayerID, uint32_t logicID, uint32_t tableID);
    void                deleteHistory( uint32_t logicID, uint32_t tableID);

	static CHTServer*		m_pSelf;
	static int			m_nRefCount;
    MySocket*          m_pSocket;
    CSqlLogicList       m_sqlLogicList;
};

#endif
