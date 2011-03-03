#include "chtserver.h"
#include "../header/defserver.h"
#include "../sql/sqltableusers.h"
#include "../socket/sendedmsg.h"

#include "chatdefs.h"
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
#ifdef MYDEBUG
    std::cerr << "CHTServer::checkParticipation()" << std::endl;
#endif
    SqlTable sqlChatTable("");
    TTable tbl;
    CMyStr strWhere = "PlayerID = " + CMyStr(_nPlayerID);
    if ( getGameOnlineUsersTable( _nLogicID, &sqlChatTable ) )
    {
        if ( !_nTableID )
        {
            if ( !sqlChatTable.Select( "InComChat", strWhere.c_str(), &tbl ) )
            {
                std::cerr << "FALSE 1" << std::endl;
                return false;
            }

            if ( tbl.empty() )
            {
                std::cerr << "FALSE 2" << std::endl;
                return false;
            }
            std::cerr << "TRUE 1" << std::endl;
            return true;
        }

        if ( !sqlChatTable.Select( "TableID", strWhere.c_str(), &tbl ) )
        {
            std::cerr << "FALSE 3" << std::endl;
            return false;
        }
        if ( tbl.empty() )
        {
            std::cerr << "FALSE 4" << std::endl;
            return false;
        }
        std::cerr << "TRUE 2" << std::endl;
        return true;

    }
    std::cerr << "FALSE 5" << std::endl;
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

bool CHTServer::getLogicChatTable( uint32_t _nLogicID, SqlTable* _pRes )
{
    CMyStr strLogicTable;

    if ( m_sqlLogicList.GetLogicName(_nLogicID, &strLogicTable) )
    {
        strLogicTable = "tb" + strLogicTable + "Chat";
        _pRes->Open(strLogicTable.c_str());

        return true;
    }
    else
        return false;
}

bool CHTServer::getBoardChatTable( uint32_t _nLogicID, SqlTable* _pRes )
{
    CMyStr strLogicTable;

    if ( m_sqlLogicList.GetLogicName(_nLogicID, &strLogicTable) )
    {
        strLogicTable = "tb" + strLogicTable + "ChatTbl";
        _pRes->Open(strLogicTable.c_str());

        return true;
    }
    else
        return false;
}

void CHTServer::joinToChat( uint32_t _nPlayerID, uint32_t _nLogicID, uint32_t _nTableID )
{
#ifdef MYDEBUG
    std::cerr << "CHTServer::joinToCommonChat() _PlayerID: " << _nPlayerID << " _LogicID: " << _nLogicID << " _nTableID: " << _nTableID << std::endl;
#endif

    SqlTable sqlGameOnlineUsersTable("");

    if ( getGameOnlineUsersTable(_nLogicID, &sqlGameOnlineUsersTable) )
    {
        TVecChar strInComChat;
        CMyStr strPlayerIDv = CMyStr(_nPlayerID);
        CMyStr strTableIDv = CMyStr(_nTableID);
        CMyStr strInComChatv;

        if (sqlGameOnlineUsersTable.SelectToStr("InComChat", "PlayerID", strPlayerIDv.c_str(), &strInComChat))
        {
            if ( _nTableID )
            {
                sqlGameOnlineUsersTable.Update("TableID", strTableIDv.c_str(), "PlayerID", strPlayerIDv.c_str());
            }
            else
            {
                strInComChatv = CMyStr("TRUE");
                sqlGameOnlineUsersTable.Update("InComChat", strInComChatv.c_str(), "PlayerID", strPlayerIDv.c_str());
            }
        }
        else
        {
            TVecMyStr fields, values;

            CMyStr strPlayerIDf = CMyStr("PlayerID");
            CMyStr strTableIDf = CMyStr("TableID");
            CMyStr strInComChatf = CMyStr("InComChat");

            fields.push_back(&strPlayerIDf);
            fields.push_back(&strTableIDf);
            fields.push_back(&strInComChatf);

            values.push_back(&strPlayerIDv);
            values.push_back(&strTableIDv);

            if ( _nTableID )
            {

                strInComChatv = CMyStr("FALSE");
            }
            else
            {
                strInComChatv = CMyStr("TRUE");
            }

            values.push_back(&strInComChatv);

            sqlGameOnlineUsersTable.Insert( fields, values );

        }

        /// send last 10 messages
        getHistory( _nPlayerID, _nLogicID, _nTableID );
    }
    else
    {
#ifdef MYDEBUG
        std::cerr << "CHTServer::joinToCommonChat() can't open DB" << std::endl;
#endif
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
            if (vecCmd.size() > sizeof(uint32_t))
            {
                vecCmd.push_back(0);
                messageToAll(_pMsg->GetTo(), (uint32_t) vecCmd.at(0), (TVecChar*)&vecCmd.at(sizeof(uint32_t)));
            }
            break;
        case CMD_TBL_CHAT_MSG:
            if (vecCmd.size() > sizeof(uint32_t)*2)
            {
                vecCmd.push_back(0);
                messageToAll( _pMsg->GetTo(), (uint32_t) vecCmd.at(0), (TVecChar*)&vecCmd.at(sizeof(uint32_t)*2), (uint32_t) (unsigned char) vecCmd.at(sizeof(uint32_t)) );
            }
            break;
        case CMD_GAME_CHAT_JOIN:
            if (vecCmd.size() == sizeof(uint32_t) )
            {
                joinToChat( _pMsg->GetTo(), (uint32_t) vecCmd.at(0) );
            }
            break;
        case CMD_GAME_CHAT_LEAVE:
            if (vecCmd.size() == sizeof(uint32_t) )
            {
                leaveChat( _pMsg->GetTo(), (uint32_t) vecCmd.at(0) );
            }
            break;
        case CMD_TBL_CHAT_JOIN:
            if (vecCmd.size() == sizeof(uint32_t) * 2 )
            {
                joinToChat( _pMsg->GetTo(), (uint32_t) vecCmd.at(0), (uint32_t)(unsigned char)  vecCmd.at(sizeof(uint32_t)) );
            }
            break;
        case CMD_TBL_CHAT_LEAVE:
            if (vecCmd.size() == sizeof(uint32_t) *2 )
            {
                leaveChat( _pMsg->GetTo(), (uint32_t) vecCmd.at(0), (uint32_t)(unsigned char)  vecCmd.at(sizeof(uint32_t)) );
            }
            break;
        default:
            break;
    }


}

void CHTServer::leaveChat( uint32_t _nPlayerID, uint32_t _nLogicID, uint32_t _nTableID )
{
#ifdef MYDEBUG
    std::cerr << "CHTServer::leaveChat() _PlayerID: " << _nPlayerID << " _LogicID: " << _nLogicID << " _nTableID: " << _nTableID << std::endl;
#endif

    SqlTable sqlGameOnlineUsersTable("");

    if ( getGameOnlineUsersTable(_nLogicID, &sqlGameOnlineUsersTable) )
    {
        TVecChar strInComChat, strInBoardChat;
        CMyStr strPlayerIDv = CMyStr(_nPlayerID);
        CMyStr strTableIDv = CMyStr(_nTableID);
        CMyStr strInComChatv, strInBoardChatv;

        if (sqlGameOnlineUsersTable.SelectToStr("InComChat", "PlayerID", strPlayerIDv.c_str(), &strInComChat))
        {
            if ( sqlGameOnlineUsersTable.SelectToStr("TableID", "PlayerID", strPlayerIDv.c_str(), &strInBoardChat))
            {
                strInComChatv = CMyStr((char*)&strInComChat[0]);
                strInBoardChatv = CMyStr((char*)&strInBoardChat[0]);
        #ifdef MYDEBUG
                std::cerr << "CHTServer::leaveChat() _nTableID:" << _nTableID << " strInComChatv: " << strInComChatv << " strInBoardChat " << strInBoardChatv << std::endl;
        #endif
                if ( _nTableID && (strInComChatv == "1") )
                {
                    sqlGameOnlineUsersTable.Update("TableID", "0", "PlayerID", strPlayerIDv.c_str());
                }
                else if ( (!_nTableID) && (strInComChatv == "1") )
                {
                    if ( (strInBoardChatv != "0") && (strInComChatv == "1") )
                    {
                        sqlGameOnlineUsersTable.Update("InComChat", "0", "PlayerID", strPlayerIDv.c_str());
                    }
                    else
                    {
                        sqlGameOnlineUsersTable.Delete("PlayerID", strPlayerIDv.c_str());
                    }
                }
                else
                {
                    sqlGameOnlineUsersTable.Delete("PlayerID", strPlayerIDv.c_str());
                }
            }
        }

    }
    else
    {
#ifdef MYDEBUG
        std::cerr << "CHTServer::leaveChat() can't open DB" << std::endl;
#endif
    }
}

void CHTServer::getHistory( uint32_t playerID, uint32_t logicID, uint32_t tableID )
{
    SqlTable chatTable("");
    TTable queryRes;
    CMyStr query;

    if(tableID)
    {
        getBoardChatTable( logicID, &chatTable );

        // select Msg from tbChessChat where TableID=tableID order by CreateTime desc limit 10;
        query = CMyStr("TableID=") + CMyStr(tableID) +
                       CMyStr(" order by CreateTime desc limit ") + MAX_CHAT_HISTORY;
    }
    else
    {
        getLogicChatTable(logicID, &chatTable); // tbChessChat

        // select Msg from tbChessChat where CreateTime>0 order by CreateTime desc limit 10;
        query = CMyStr("CreateTime>0 order by CreateTime desc limit ") + MAX_CHAT_HISTORY;
    }

    chatTable.Select("Msg", query.c_str(), &queryRes);

    // send messages in reverse order
    for(int i=queryRes.size()-1; i>=0; i--)
    {
        sendMsgToOne( playerID, logicID, &queryRes[i][0], tableID );
    }

}



void CHTServer::messageToAll( uint32_t _nPlayerID, uint32_t _nLogicID,
                                const TVecChar* _vecData, uint32_t _nTableID )
{
#ifdef MYDEBUG
    std::cout << "CHTServer::messageToAll()" << std::endl;
#endif
    if (!checkParticipation(_nPlayerID, _nLogicID, _nTableID) )
    {
        return;
    }

    SqlTable sqlChatTable("");

    if ( _vecData->empty() )
    {
#ifdef MYDEBUG
        std::cout << "CHTServer::messageToAll() _vecData is empty" << std::endl;
#endif
        return;
    }

    CMyStr strMsg = CMyStr((char*)_vecData);

    if ( _nTableID )
    {
        getBoardChatTable( _nLogicID, &sqlChatTable );

        TVecMyStr parameters;
        CMyStr tableID = CMyStr( _nTableID );
        parameters.push_back(&tableID);
        parameters.push_back(&strMsg);
        parameters.push_back(&MAX_CHAT_HISTORY);
        sqlChatTable.Call("AddToHistoryTbl", parameters);
    }
    else
    {
        getLogicChatTable(_nLogicID, &sqlChatTable);

        TVecMyStr parameters;
        parameters.push_back(&strMsg);
        parameters.push_back(&MAX_CHAT_HISTORY);
        sqlChatTable.Call("AddToHistory", parameters);
    }

    TVecChar vecChar;
    SqlTableUsers wsUsers;

    if ( wsUsers.GetUserName( _nPlayerID, &vecChar ) )
    {
        CMyStr strUserName = CMyStr(&vecChar);
        strMsg = "'" + strUserName + ": " + strMsg + "'";
    }


    sendMsgToAll( _nLogicID, &strMsg, _nTableID );

}

void CHTServer::sendMsgToAll( uint32_t _nLogicID, CMyStr* _strMsg, uint32_t _nTableID )
{
#ifdef MYDEBUG
    std::cout << "CHTServer::sendMsgToAll()" << std::endl;
#endif

    SqlTable sqlChatTable("");
    TTable tbl;

    if ( getGameOnlineUsersTable( _nLogicID, &sqlChatTable ) )
    {
        std::cout << "CHTServer::sendMsgToAll() 1" << std::endl;

        CMyStr strWhere;
        CSendedMsg sendedMsg;

        if ( _nTableID )
        {
            sendedMsg.addData((char)ANS_MSG_TBL);
            strWhere = "TableID = " + CMyStr(_nTableID);
        }
        else
        {
            sendedMsg.addData((char)ANS_MSG);
            strWhere = "InComChat = 1";
        }

        sqlChatTable.Select( "PlayerID", strWhere.c_str(), &tbl );


        sendedMsg.addData(_strMsg);

#ifdef MYDEBUG
        std::cout << "CHTServer::sendMsgToAll() nTo: " << std::endl;
#endif

        for(TTable::const_iterator it = tbl.begin(); it != tbl.end(); ++it)
        {
            uint32_t nTo = atoi(it->at(0).c_str());
#ifdef MYDEBUG
            std::cout << nTo << " ";
#endif
            std::cout << "CHTServer::sendMsgToAll() 2 " << std::endl;

            sendMsg( nTo, &sendedMsg );
        }
#ifdef MYDEBUG
        std::cout << std::endl;
#endif

        std::cout << "CHTServer::sendMsgToAll() 3 " << std::endl;

    }

}

void CHTServer::sendMsgToOne( uint32_t _nPlayerID, uint32_t _nLogicID, CMyStr* _strMsg, uint32_t _nTableID  )
{

        CSendedMsg sendedMsg;

        if ( _nTableID )
        {
            sendedMsg.addData((char)ANS_MSG_TBL);
        }
        else
        {
            sendedMsg.addData((char)ANS_MSG);
        }

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

