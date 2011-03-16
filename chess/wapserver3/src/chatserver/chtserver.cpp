#include "chtserver.h"
#include "../header/defserver.h"
#include "../header/defservice.h"
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

bool CHTServer::checkParticipation( uint32_t playerID, uint32_t logicID, uint32_t tableID )
{
    SqlTable sqlGameOnlineUsersTable("");
    if ( getGameOnlineUsersTable(logicID, &sqlGameOnlineUsersTable) )
    {
        CMyStr query = CMyStr("PlayerID = ") + CMyStr(playerID) + CMyStr(" AND TableID = ") + CMyStr(tableID);

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



bool CHTServer::getGameOnlineUsersTable( uint32_t logicID, SqlTable* _pRes )
{
    CMyStr strLogicTable;

    if ( m_sqlLogicList.GetLogicName(logicID, &strLogicTable) )
    {
        strLogicTable = "tb" + strLogicTable + "ChatUserOnline";
        _pRes->Open(strLogicTable.c_str());

        return true;
    }
    else
        return false;

}

bool CHTServer::getChatTable( uint32_t logicID, SqlTable* _pRes )
{
    CMyStr strLogicTable;

    if ( m_sqlLogicList.GetLogicName(logicID, &strLogicTable) )
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

void CHTServer::newMsg( ClientMsg* _pMsg )
{
    TVecChar vecCmd;
    _pMsg->GetData( ClientMsg::etpCommand, &vecCmd );

    std::cout << "--- INCOMING MSG --- FROM: " << ( uint32_t )_pMsg->GetTo() <<
                 ";  " << GlobalServer::commandToString( _pMsg->GetCommand()) <<
                 ";  ";

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
            if (vecCmd.size() == sizeof(uint32_t) )
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

void CHTServer::sendServerNote(int cmd, uint32_t playerID, uint32_t logicID, uint32_t tableID)
{
    TVecChar vecChar;
    SqlTableUsers wsUsers;
    if ( wsUsers.GetUserName( playerID, &vecChar ) ) // add a user name into the message
    {
        CMyStr userName(&vecChar);
        sendMsgToAll(cmd, logicID, &userName, tableID);
    }
}

void CHTServer::sendUserNames(uint32_t playerID, uint32_t logicID, uint32_t tableID)
{
    SqlTable sqlGameOnlineUsersTable("");
    if (!getGameOnlineUsersTable(logicID, &sqlGameOnlineUsersTable) )
    {
        return;
    }

    CMyStr onlineUsersTbl = sqlGameOnlineUsersTable.getTableName();

    SqlTableUsers wsUsers;
    CMyStr usersTbl = wsUsers.getTableName();

    // Get names of online users
    // select wsUsers.User from wsUsers inner join tbChessChatUserOnline on wsUsers.GUID=tbChessChatUserOnline.PlayerID
    // where tbChessChatUserOnline.TableID = 0
    CMyStr query = "select " + usersTbl +".User from " + usersTbl + " inner join " + onlineUsersTbl +
                   " on " + usersTbl + ".GUID=" + onlineUsersTbl + ".PlayerID " +
                   " where " + onlineUsersTbl + ".TableID=" + CMyStr(tableID);
    TTable tbl;
    sqlGameOnlineUsersTable.Query(query.c_str(), &tbl);


    for (int i=0; i<tbl.size(); i++ )
    {
        CMyStr userName = tbl[i][0];

        sendMsgToOne(ANS_CHAT_USER_ONLINE, playerID, logicID, &userName);
    }



}


void CHTServer::joinChat( uint32_t playerID, uint32_t logicID, uint32_t tableID )
{

    if ( !checkParticipation(playerID, logicID, tableID ))
    {
        // send existing names to the new user
        sendUserNames(playerID, logicID, tableID);

        /// send last 10 messages to the new user
        sendHistory( playerID, logicID, tableID );

        // insert the new user
        SqlTable sqlGameOnlineUsersTable("");
        if ( !getGameOnlineUsersTable(logicID, &sqlGameOnlineUsersTable) )
        {
            return;
        }

        TVecMyStr fields, values;

        CMyStr strPlayerIDf = CMyStr("PlayerID");
        CMyStr strTableIDf = CMyStr("TableID");
        fields.push_back(&strPlayerIDf);
        fields.push_back(&strTableIDf);

        CMyStr strPlayerIDv = CMyStr(playerID);
        CMyStr strTableIDv = CMyStr(tableID);
        values.push_back(&strPlayerIDv);
        values.push_back(&strTableIDv);

        sqlGameOnlineUsersTable.Delete("PlayerID", CMyStr(playerID).c_str());
        sqlGameOnlineUsersTable.Insert( fields, values );

        // send USER_JOINED to all
        sendServerNote(ANS_CHAT_USER_JOINED, playerID, logicID, tableID);

    }

}

void CHTServer::leaveChat( uint32_t playerID, uint32_t logicID)
{
    uint32_t tableID = 0;
    SqlTable sqlGameOnlineUsersTable("");
    if ( getGameOnlineUsersTable(logicID, &sqlGameOnlineUsersTable) )
    {
        TTable tbl;
        CMyStr query = CMyStr("PlayerID = ") + CMyStr(playerID);
        sqlGameOnlineUsersTable.Select("TableID", query.c_str(), &tbl);

        if(!tbl.empty())
        {
            tableID = atoi(tbl[0][0].c_str());
        }


        sqlGameOnlineUsersTable.Delete("PlayerID", CMyStr(playerID).c_str());

        sendServerNote(ANS_CHAT_USER_LEFT, playerID, logicID, tableID);
    }
}


void CHTServer::sendHistory( uint32_t playerID, uint32_t logicID, uint32_t tableID )
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
        sendMsgToOne( ANS_CHAT_MSG, playerID, logicID, &queryRes[i][0] );
    }

}

void CHTServer::deleteHistory( uint32_t logicID, uint32_t tableID)
{
    SqlTable chatTable("");
    getChatTable( logicID, &chatTable );

  //  CMyStr query = CMyStr("TableID=") + CMyStr(tableID);
    chatTable.Delete("TableID", CMyStr(tableID).c_str());
}


void CHTServer::messageToAll( uint32_t playerID, uint32_t logicID,
                                const TVecChar* _vecData, uint32_t tableID )
{
    if (!checkParticipation(playerID, logicID, tableID) )
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
    if ( wsUsers.GetUserName( playerID, &vecChar ) )
    {
        CMyStr strUserName = CMyStr(&vecChar);
        strMsg = strUserName + ": " + strMsg;
    }


    // save message into the history
    getChatTable( logicID, &sqlChatTable );

    TVecMyStr parameters;
    CMyStr tableIDstr = CMyStr( tableID );
    parameters.push_back(&tableIDstr);
    parameters.push_back(&strMsg);
    parameters.push_back(&MAX_CHAT_HISTORY);
    sqlChatTable.Call("AddToHistory", parameters);


    // and send it
    sendMsgToAll(ANS_CHAT_MSG, logicID, &strMsg, tableID );

}

void CHTServer::sendMsgToAll( int cmd, uint32_t logicID, CMyStr* _strMsg, uint32_t tableID )
{

    SqlTable sqlChatTable("");
    TTable tbl;

    if ( getGameOnlineUsersTable( logicID, &sqlChatTable ) )
    {
        std::cout << "CHTServer::sendMsgToAll() 1" << std::endl;

        CSendedMsg sendedMsg;
        sendedMsg.addData((char)cmd);
        sendedMsg.addData(_strMsg);

        // fetch all the chat members and send the message them all
        CMyStr strWhere = "TableID = " + CMyStr(tableID);
        sqlChatTable.Select( "PlayerID", strWhere.c_str(), &tbl );

        for(TTable::const_iterator it = tbl.begin(); it != tbl.end(); ++it)
        {
            uint32_t nTo = atoi(it->at(0).c_str());
            sendMsg( nTo, &sendedMsg );
        }

    }

}

void CHTServer::sendMsgToOne( int cmd, uint32_t playerID, uint32_t logicID, CMyStr* _strMsg)
{
    CSendedMsg sendedMsg;

    sendedMsg.addData((char)cmd);
    sendedMsg.addData(_strMsg);

    sendMsg( playerID, &sendedMsg );
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

