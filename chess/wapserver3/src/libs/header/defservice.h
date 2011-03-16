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

// CHESS SERVER MESSAGES 80 -
const int		CMD_JOIN	= 81;
const int		CMD_STEP	= 82;
const int		CMD_DRAW	= 83;
const int		ANS_JOIN	= 84;
const int		ANS_STEP	= 85;
const int		ANS_DRAW	= 86;
const int		ANS_END		= 87;
const int		ANS_START	= 88;
const int		CMD_GET_FIELD	= 89;
const int		CMD_LOOSE	= 90;
const int    	ANS_FIELD	= 91;
const int		ANS_OSTEP	= 92;
const int		ANS_OPPONENT	= 93;
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






#endif /*DEFSERVICE_H*/
