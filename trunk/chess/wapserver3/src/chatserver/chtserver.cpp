#include "chtserver.h"
#include "../header/defserver.h"
#include "../sql/sqltableusers.h"
#include "../socket/sendedmsg.h"

#include <stdlib.h>
#include <sstream>
#include <string>

#define MYDEBUG

using namespace std;

const CMyStr MAX_CHAT_HISTORY = "10";

CHTServer* CHTServer::m_pSelf = 0;
int CHTServer::m_nRefCount = 0;

CHTServer* CHTServer::Instance()
{
	if( m_pSelf == 0 )
		m_pSelf = new CHTServer;

	++m_nRefCount;

	return m_pSelf;
}

void CHTServer::FreeInst()
{
	--m_nRefCount;

	if( m_nRefCount > 0 )
		return;

	KillObject();
}

void CHTServer::KillObject()
{
	if( m_pSelf != 0 )
		delete m_pSelf;

	m_pSelf = 0;
	m_nRefCount = 0;
}

CHTServer::CHTServer()
    :AccessInfo( "cht", "*EF73C5727E601156FC22161D49B42AC88324678C" )
{

}

CHTServer::~CHTServer()
{

}

void CHTServer::DoAllMsg( MySocket* _pSocket )
{
	ClientMsg inMsg;
    setSocket( _pSocket );

	while( _pSocket->GetMsg( inMsg ) )
	{
		switch( inMsg.GetTo() )
		{
		case SRV:
            break;
		default:	newMsg( &inMsg );
		};
	}
}

bool CHTServer::checkParticipation( uint32_t _nPlayerID, uint32_t _nLogicID, uint32_t _nTableID )
{
    SqlTable sqlGameOnlineUsersTable("");
    if ( getGameOnlineUsersTable(_nLogicID, &sqlGameOnlineUsersTable) )
    {
        CMyStr query = CMyStr("PlayerID = ") + CMyStr(_nPlayerID) + CMyStr(" AND TableID = ") + CMyStr(_nTableID);

        TTable tbl;
        if (!sqlGameOnlineUsersTable.Select("*", query.c_str(), &tbl))
        {
            return false;
        }

        if ( tbl.empty() )
        {
            return false;
        }

        return true;
    }

    return false;
}



bool CHTServer::getGameOnlineUsersTable( uint32_t _nLogicID, SqlTable* _pRes )
{
    CMyStr strLogicTable;

    if ( m_sqlLogicList.GetLogicName(_nLogicID, &strLogicTable) )
    {
        strLogicTable = "tb" + strLogicTable + "ChatUserOnline";
        _pRes->Open(strLogicTable.c_str());

        return true;
    }
    else
        return false;

}

bool CHTServer::getChatTable( uint32_t _nLogicID, SqlTable* _pRes )
{
    CMyStr strLogicTable;

    if ( m_sqlLogicList.GetLogicName(_nLogicID, &strLogicTable) )
    {
        strLogicTable = "tb" + strLogicTable + "Chat";
        _pRes->Open(strLogicTable.c_str());

        return true;
    }
    else
    {
        return false;
    }
}

void CHTServer::joinChat( uint32_t _nPlayerID, uint32_t _nLogicID, uint32_t _nTableID )
{

    if ( !checkParticipation(_nPlayerID, _nLogicID, _nTableID ))
    {
        SqlTable sqlGameOnlineUsersTable("");
        if ( !getGameOnlineUsersTable(_nLogicID, &sqlGameOnlineUsersTable) )
        {
            return;
        }

        TVecMyStr fields, values;

        CMyStr strPlayerIDf = CMyStr("PlayerID");
        CMyStr strTableIDf = CMyStr("TableID");
        fields.push_back(&strPlayerIDf);
        fields.push_back(&strTableIDf);

        CMyStr strPlayerIDv = CMyStr(_nPlayerID);
        CMyStr strTableIDv = CMyStr(_nTableID);
        values.push_back(&strPlayerIDv);
        values.push_back(&strTableIDv);

        sqlGameOnlineUsersTable.Insert( fields, values );

        /// send last 10 messages
        getHistory( _nPlayerID, _nLogicID, _nTableID );
    }

}

void CHTServer::newMsg( ClientMsg* _pMsg )
{
    TVecChar vecCmd;
    _pMsg->GetData( ClientMsg::etpCommand, &vecCmd );

    std::cout << "--- INCOMING MSG --- FROM: " << ( uint32_t )_pMsg->GetTo() <<
                  ";  COMMAND: " << ( uint32_t )(unsigned char) _pMsg->GetCommand() <<
                  ";  DATA: ";
    for ( int i = 0; i < vecCmd.size(); ++i )
    {
        std::cout << (int) vecCmd[i] << " ";
    }
    std::cout << std::endl;


    switch( _pMsg->GetCommand() )
    {
        case CMD_CHAT_MSG:
            if (vecCmd.size() > sizeof(uint32_t)*2)
            {
                vecCmd.push_back(0);
                messageToAll( _pMsg->GetTo(), (uint32_t) vecCmd.at(0), (TVecChar*)&vecCmd.at(sizeof(uint32_t)*2), (uint32_t) (unsigned char) vecCmd.at(sizeof(uint32_t)) );
            }
            break;
        case CMD_CHAT_JOIN:
            if (vecCmd.size() == sizeof(uint32_t) * 2 )
            {
                joinChat( _pMsg->GetTo(), (uint32_t) vecCmd.at(0), (uint32_t)(unsigned char)  vecCmd.at(sizeof(uint32_t)) );
            }
            break;
        case CMD_CHAT_LEAVE:
            if (vecCmd.size() == sizeof(uint32_t) *2 )
            {
                leaveChat( _pMsg->GetTo(), (uint32_t) vecCmd.at(0) );
            }
            break;
        case CMD_CHAT_DELETE_HISTORY:
            if (vecCmd.size() == sizeof(uint32_t) *2 )
            {
                deleteHistory( (uint32_t) vecCmd.at(0), (uint32_t)(unsigned char)  vecCmd.at(sizeof(uint32_t)) );
            }
            break;
        default:
            break;
    }


}

void CHTServer::leaveChat( uint32_t _nPlayerID, uint32_t _nLogicID )
{
    SqlTable sqlGameOnlineUsersTable("");
    if ( getGameOnlineUsersTable(_nLogicID, &sqlGameOnlineUsersTable) )
    {
        sqlGameOnlineUsersTable.Delete("PlayerID", CMyStr(_nPlayerID).c_str());
    }
}


void CHTServer::getHistory( uint32_t playerID, uint32_t logicID, uint32_t tableID )
{
    SqlTable chatTable("");
    getChatTable( logicID, &chatTable );

    // select Msg from tbChessChat where TableID=tableID order by CreateTime desc limit 10;
    CMyStr query = CMyStr("TableID=") + CMyStr(tableID) +
                   CMyStr(" order by CreateTime desc limit ") + MAX_CHAT_HISTORY;

    TTable queryRes;
    chatTable.Select("Msg", query.c_str(), &queryRes);

    // send messages in reverse order
    for(int i=queryRes.size()-1; i>=0; i--)
    {
        sendMsgToOne( playerID, logicID, &queryRes[i][0], tableID );
    }

}

void CHTServer::deleteHistory( uint32_t logicID, uint32_t tableID)
{
    SqlTable chatTable("");
    getChatTable( logicID, &chatTable );

  //  CMyStr query = CMyStr("TableID=") + CMyStr(tableID);
    chatTable.Delete("TableID", CMyStr(tableID).c_str());
}


void CHTServer::messageToAll( uint32_t _nPlayerID, uint32_t _nLogicID,
                                const TVecChar* _vecData, uint32_t _nTableID )
{
    if (!checkParticipation(_nPlayerID, _nLogicID, _nTableID) )
    {
        return;
    }

    SqlTable sqlChatTable("");

    if ( _vecData->empty() )
    {
        return;
    }

    CMyStr strMsg = CMyStr((char*)_vecData);

    // add a user name into the message
    TVecChar vecChar;
    SqlTableUsers wsUsers;
    if ( wsUsers.GetUserName( _nPlayerID, &vecChar ) )
    {
        CMyStr strUserName = CMyStr(&vecChar);
        strMsg = strUserName + ": " + strMsg;
    }


    // save message into the history
    getChatTable( _nLogicID, &sqlChatTable );

    TVecMyStr parameters;
    CMyStr tableID = CMyStr( _nTableID );
    parameters.push_back(&tableID);
    parameters.push_back(&strMsg);
    parameters.push_back(&MAX_CHAT_HISTORY);
    sqlChatTable.Call("AddToHistory", parameters);


    // and send it
    sendMsgToAll( _nLogicID, &strMsg, _nTableID );

}

void CHTServer::sendMsgToAll( uint32_t _nLogicID, CMyStr* _strMsg, uint32_t _nTableID )
{

    SqlTable sqlChatTable("");
    TTable tbl;

    if ( getGameOnlineUsersTable( _nLogicID, &sqlChatTable ) )
    {
        std::cout << "CHTServer::sendMsgToAll() 1" << std::endl;

        CSendedMsg sendedMsg;
        sendedMsg.addData((char)ANS_CHAT_MSG);
        sendedMsg.addData(_strMsg);

        // fetch chat members and send the message them all
        CMyStr strWhere = "TableID = " + CMyStr(_nTableID);
        sqlChatTable.Select( "PlayerID", strWhere.c_str(), &tbl );

        for(TTable::const_iterator it = tbl.begin(); it != tbl.end(); ++it)
        {
            uint32_t nTo = atoi(it->at(0).c_str());
            sendMsg( nTo, &sendedMsg );
        }

    }

}

void CHTServer::sendMsgToOne( uint32_t _nPlayerID, uint32_t _nLogicID, CMyStr* _strMsg, uint32_t _nTableID  )
{
    CSendedMsg sendedMsg;

    sendedMsg.addData((char)ANS_CHAT_MSG);
    sendedMsg.addData(_strMsg);

    sendMsg( _nPlayerID, &sendedMsg );
}

void CHTServer::sendMsg( uint32_t _nTo, CSendedMsg *_pMsg )
{
    ClientMsg Msg;
    TVecChar vecCmd = _pMsg->getDataVec();
    Msg.InitMsg(_nTo, vecCmd);
    m_pSocket->AddMsg(Msg);
}


void CHTServer::setSocket( MySocket * _pSocket )
{
    m_pSocket = _pSocket;
}

