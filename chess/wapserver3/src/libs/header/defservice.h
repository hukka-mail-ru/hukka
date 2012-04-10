#ifndef DEFSERVICE_H
#define DEFSERVICE_H

#include <sstream>
#include <string>

const unsigned  INVALID_TIME = 0xFFFFFFFF;
const int       DEALER_PERCENT = 10; // PERCENT
const int       MIN_DEALER_PRIZE = 1; // RUR

const int       NO_SUCH_PIN = -1; // invalid Pin on calling ReplenishBalance
const int       QUERY_ERROR = -10;

const int		ST_CREATION	= 0;
const int		ST_OPEN		= 1;
const int		ST_FULL		= 2;
const int		ST_GAME		= 3;
//const int		ST_WIN_X	= 4;
//const int		ST_DRAW		= 5;
//const int		ST_WIN_0	= 6;
//const int		ST_NO_RES	= 7;


// server commands
// TODO check magic numbers inside
const int CMD_LOGIN                      = 1;
const int CMD_REG                        = 1;

// server replies
const int LOGIN_STATUS                   = 1;
const int REG_STATUS                     = 1;


// CHAT SERVER MESSAGES 20 -
const int       CMD_CHAT_MSG            = 20;
const int       CMD_CHAT_JOIN           = 21;
const int       CMD_CHAT_LEAVE          = 22;
const int       CMD_CHAT_DELETE_HISTORY = 23;
const int       ANS_CHAT_MSG            = 24;
const int       ANS_CHAT_USER_ONLINE    = 25;
const int       ANS_CHAT_USER_JOINED    = 26;
const int       ANS_CHAT_USER_LEFT      = 27;

// TABLE MANAGER 50 -
const int CMD_CREATE         = 51;
const int CMD_RANDOM_OP      = 52;
const int CMD_GETMYTBL       = 53;
const int CMD_FIND           = 54;
const int CMD_GET_PARAMS     = 55;
const int CMD_SET_PARAMS     = 56;
const int CMD_DELETE         = 57;
const int ANS_CREATE         = 58;
const int ANS_TABLE          = 59;
const int ANS_MYTBL          = 60;
const int ANS_RANDOM_OP      = 61;
const int ANS_GET_PARAMS     = 62;
const int ANS_SET_PARAMS     = 63;
const int ANS_DELETE         = 64;
const int CMD_REPLENISH      = 65;
const int ANS_REPLENISH      = 66;


// CHESS SERVER MESSAGES 80 -
const int		CMD_JOIN	= 81;
const int		CMD_MOVE	= 82;
const int		CMD_DRAW	= 83;
const int		ANS_JOIN	= 84;
const int		ANS_MOVE	= 85;
const int		ANS_DRAW	= 86;
const int		ANS_END		= 87;
const int		ANS_START	= 88;
const int		CMD_GET_POSITION	= 89;
const int		CMD_SURRENDER	= 90;
const int    	ANS_POSITION	= 91;
const int		ANS_OSTEP	= 92;
const int		ANS_OPPONENT_JOINED	= 93;
const int		CMD_OPAGREE = 94;
const int		CMD_OPREJECT = 95;
const int		ANS_OPREJECT = 96;
const int       CMD_DRAGREE = 97;
const int       CMD_CHECK_TIME         = 98;
const int       ANS_CHECK_TIME_NOT_SET = 99;
const int       ANS_CHECK_TIME_STEP    = 100;
const int       ANS_CHECK_TIME_GAME    = 101;
const int       ANS_OPAGREE_FAILED     = 102;
const int       CMD_TIMEOUT            = 103;
const int       CMD_RATING             = 104;
const int       ANS_RATING             = 105;
const int       CMD_GET_OPPONENT       = 106;
const int       ANS_GET_OPPONENT       = 107;
const int       CMD_LAST_GAME_RESULT   = 108;
const int       ANS_LAST_GAME_RESULT   = 109;
const int       CMD_DELETE_LAST_GAME_RESULT = 110;
const int       CMD_BALANCE             = 111;
const int       ANS_BALANCE             = 112;

const int       P_NONE      = 0;
const int		P_DONE		= 20;
const int		P_FAILED	= 21;
const int		P_VALID		= 22;
const int		P_NOT_VALID	= 23;
const int		P_YES		= 24;
const int		P_NO		= 25;
const int		P_WIN		= 26;
const int		P_LOOSE		= 27;
const int		P_LOOSE_TIME = 28;
const int		P_XPLAYER	= 29;
const int		P_OPLAYER	= 30;
const int		P_DRAW		= 31;
const int		P_FINISH    = 32;
const int       P_OFFER     = 33;
const int       P_ACCEPT    = 34;
const int       P_NOTALLOWED = 35;
const int       P_REJECT    = 36;
const int       P_WAIT      = 37;
const int		P_WIN_TIME  = 38;
const int		P_NOT_FULL  = 39;
const int       P_NO_RES    = 40;
const int       P_WIN_SURRENDER = 41;
const int       P_LOOSE_SURRENDER = 42;
const int       P_DRAW_STALEMATE = 43;
const int       P_DRAW_TRIPPLE_OCCURRENCE = 44;
const int       P_DRAW_FIFTY_MOVES = 45;

// game table parameter ids (see tbParameterList table)
const int  PARAMETER_ID_PLAYER_NAME      =  0;
const int  PARAMETER_ID_OPPONENT_NAME    =  1;
const int  PARAMETER_ID_PASSWD           =  2;
const int  PARAMETER_ID_MOVETIME         =  3;
const int  PARAMETER_ID_GAMETIME         =  4;
const int  PARAMETER_ID_MINRATING        =  5;
const int  PARAMETER_ID_MAXRATING        =  6;
const int  PARAMETER_ID_BET              =  7;


class GlobalServer
{
public:
    static std::string commandToString(char command)
    {
       std::string res;

       switch(command)
       {
               // case CMD_LOGIN: res = "CMD_LOGIN"; break;
               // case CMD_REG:   res =  "CMD_REG"; break;

                // CHAT
                case ANS_CHAT_MSG:             res = "ANS_CHAT_MSG"; break;
                case ANS_CHAT_USER_ONLINE:     res = "ANS_CHAT_USER_ONLINE"; break;
                case ANS_CHAT_USER_JOINED:     res = "ANS_CHAT_USER_JOINED"; break;
                case ANS_CHAT_USER_LEFT:       res = "ANS_CHAT_USER_LEFT"; break;
                case CMD_CHAT_MSG :            res = "CMD_CHAT_MSG "; break;
                case CMD_CHAT_JOIN :           res = "CMD_CHAT_JOIN "; break;
                case CMD_CHAT_LEAVE :          res = "CMD_CHAT_LEAVE "; break;
                case CMD_CHAT_DELETE_HISTORY:  res = "CMD_CHAT_DELETE_HISTORY "; break;

                // TBM
                case CMD_CREATE:      res = "CMD_CREATE"; break;
                case CMD_RANDOM_OP :  res = "CMD_RANDOM_OP"; break;
                case CMD_GETMYTBL :   res = "CMD_GETMYTBL"; break;
                case CMD_FIND :       res = "CMD_FIND"; break;
                case CMD_GET_PARAMS : res = "CMD_GET_PARAMS"; break;
                case CMD_SET_PARAMS : res = "CMD_SET_PARAMS"; break;
                case CMD_DELETE :     res = "CMD_DELETE"; break;
                case ANS_CREATE :     res = "ANS_CREATE"; break;
                case ANS_TABLE  :     res = "ANS_TABLE"; break;
                case ANS_MYTBL   :     res = "ANS_MYTBL"; break;
                case ANS_RANDOM_OP  :     res = "ANS_RANDOM_OP"; break;
                case ANS_GET_PARAMS  :     res = "ANS_GET_PARAMS"; break;
                case ANS_SET_PARAMS  :     res = "ANS_SET_PARAMS"; break;
                case ANS_DELETE      :     res = "ANS_DELETE"; break;
                case CMD_REPLENISH :     res = "CMD_REPLENISH"; break;
                case ANS_REPLENISH :     res = "ANS_REPLENISH"; break;

                // CHS
                case CMD_JOIN:  res = "CMD_JOIN"; break;
                case CMD_MOVE:  res = "CMD_MOVE"; break;
                case CMD_DRAW:  res = "CMD_DRAW"; break;
                case CMD_GET_POSITION    : res = "CMD_GET_POSITION"; break;
                case CMD_SURRENDER     : res = "CMD_SURRENDER"; break;
                case CMD_OPAGREE   : res = "CMD_OPAGREE"; break;
                case CMD_OPREJECT  : res = "CMD_OPREJECT"; break;
                case CMD_DRAGREE   : res = "CMD_DRAGREE"; break;
                case CMD_CHECK_TIME   : res = "CMD_CHECK_TIME"; break;
                case CMD_TIMEOUT   : res = "CMD_TIMEOUT"; break;
                case ANS_JOIN    : res = "ANS_JOIN"; break;
                case ANS_MOVE     : res = "ANS_MOVE"; break;
                case ANS_DRAW     : res = "ANS_DRAW"; break;
                case ANS_END      : res = "ANS_END"; break;
                case ANS_START    : res = "ANS_START"; break;
                case ANS_POSITION    : res = "ANS_POSITION"; break;
                case ANS_OSTEP    : res = "ANS_OSTEP"; break;
                case ANS_OPPONENT_JOINED     : res = "ANS_OPPONENT_JOINED"; break;
                case ANS_OPREJECT  : res = "ANS_OPREJECT"; break;
                case ANS_CHECK_TIME_NOT_SET  : res = "ANS_CHECK_TIME_NOT_SET"; break;
                case ANS_CHECK_TIME_STEP  : res = "ANS_CHECK_TIME_STEP"; break;
                case ANS_CHECK_TIME_GAME  : res = "ANS_CHECK_TIME_GAME"; break;
                case ANS_OPAGREE_FAILED  : res = "ANS_OPAGREE_FAILED"; break;
                case CMD_RATING  : res = "CMD_RATING"; break;
                case ANS_RATING  : res = "ANS_RATING"; break;
                case CMD_BALANCE  : res = "CMD_BALANCE"; break;
                case ANS_BALANCE  : res = "ANS_BALANCE"; break;
                case CMD_GET_OPPONENT  : res = "CMD_GET_OPPONENT"; break;
                case ANS_GET_OPPONENT  : res = "ANS_GET_OPPONENT"; break;
                case CMD_LAST_GAME_RESULT  : res = "CMD_LAST_GAME_RESULT"; break;
                case ANS_LAST_GAME_RESULT  : res = "ANS_LAST_GAME_RESULT"; break;
                case CMD_DELETE_LAST_GAME_RESULT  : res = "CMD_DELETE_LAST_GAME_RESULT"; break;

                default:        res = "UNKNOWN";break;
        }

        std::stringstream out;
        out << (int)(unsigned char)command;

        res += " (" + out.str() + ")";

        return res;
    }

};



#endif /*DEFSERVICE_H*/
