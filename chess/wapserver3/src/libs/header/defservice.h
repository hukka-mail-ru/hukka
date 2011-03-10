#ifndef DEFSERVICE_H
#define DEFSERVICE_H

const int		ST_CREATION	= 0;
const int		ST_OPEN		= 1;
const int		ST_FULL		= 2;
const int		ST_GAME		= 3;
const int		ST_WIN_X	= 4;
const int		ST_DRAW		= 5;
const int		ST_WIN_0	= 6;
const int		ST_NO_RES	= 7;

const int		CMD_JOIN	= 1;
const int		CMD_STEP	= 2;
const int		CMD_DRAW	= 3;
const int		ANS_JOIN	= 4;
const int		ANS_STEP	= 5;
const int		ANS_DRAW	= 6;
const int		ANS_END		= 7;
const int		ANS_START	= 8;
const int		CMD_GET_FIELD	= 9;
const int		CMD_LOOSE	= 10;
const int    	ANS_FIELD	= 11;
const int		ANS_OSTEP	= 12;
const int		ANS_OPPONENT	= 13;
const int		CMD_OPAGREE = 14;
const int		CMD_OPREJECT = 15;
const int		ANS_OPREJECT = 16;
const int     CMD_DRAGREE = 17;
const int     CMD_CHECK_TIME  = 18;
const int     ANS_CHECK_TIME_NOT_SET = 19;
const int     ANS_CHECK_TIME_STEP = 20;
const int     ANS_CHECK_TIME_GAME = 21;
const int     ANS_OPAGREE_FAILED = 22;
const int     CMD_TIMEOUT  = 23;
const int       CMD_RATING  = 24;
const int       ANS_RATING  = 25;

const int		P_DONE		= 100;
const int		P_FAILED	= 101;
const int		P_VALID		= 102;
const int		P_NOT_VALID	= 103;
const int		P_YES		= 104;
const int		P_NO		= 105;
const int		P_WIN		= 106;
const int		P_LOOSE		= 107;
const int		P_LOOSE_TIME = 108;
const int		P_XPLAYER	= 109;
const int		P_OPLAYER	= 110;
const int		P_DRAW		= 111;
const int		P_FINISH    = 112;
const int     P_OFFER     = 113;
const int     P_ACCEPT    = 114;
const int     P_NOTALLOWED = 115;
const int     P_REJECT    = 116;
const int     P_WAIT      = 117;
const int		P_WIN_TIME  = 118;
const int		P_NOT_FULL  = 119;



#endif /*DEFSERVICE_H*/
