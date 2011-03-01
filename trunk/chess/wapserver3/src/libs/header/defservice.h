#ifndef DEFSERVICE_H
#define DEFSERVICE_H

//TODO convert to Enum
#define		ST_CREATION	0
#define		ST_OPEN		1
#define		ST_FULL		2
#define		ST_GAME		3
#define		ST_WIN_X	4
#define		ST_DRAW		5
#define		ST_WIN_0	6
#define		ST_NO_RES	7

#define		CMD_JOIN	1
#define		CMD_STEP	2
#define		CMD_DRAW	3
#define		ANS_JOIN	4
#define		ANS_STEP	5
#define		ANS_DRAW	6
#define		ANS_END		7
#define		ANS_START	8
#define		CMD_GET_FIELD	9
#define		CMD_LOOSE	10
#define 	ANS_FIELD	11
#define		ANS_OSTEP	12
#define		ANS_OPPONENT	13
#define		CMD_OPAGREE 14
#define		CMD_OPREJECT 15
#define		ANS_OPREJECT 16
#define     CMD_DRAGREE 17
#define     CMD_CHECK_TIME  18
#define     ANS_CHECK_TIME_NOT_SET 19
#define     ANS_CHECK_TIME_STEP 20
#define     ANS_CHECK_TIME_GAME 21
#define     ANS_OPAGREE_FAILED 22
#define     CMD_TIMEOUT  23

#define		P_DONE		100
#define		P_FAILED	101
#define		P_VALID		102
#define		P_NOT_VALID	103
#define		P_YES		104
#define		P_NO		105
#define		P_WIN		106
#define		P_LOOSE		107
#define		P_LOOSE_TIME 108
#define		P_XPLAYER	109
#define		P_OPLAYER	110
#define		P_DRAW		111
#define		P_FINISH    112
#define     P_OFFER     113
#define     P_ACCEPT    114
#define     P_NOTALLOWED 115
#define     P_REJECT    116
#define     P_WAIT      117
#define		P_WIN_TIME  118
#define		P_NOT_FULL  119



#endif /*DEFSERVICE_H*/
