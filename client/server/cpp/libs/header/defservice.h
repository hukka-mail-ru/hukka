#ifndef DEFSERVICE_H
#define DEFSERVICE_H

#define		ST_CREATION		0
#define		ST_OPEN			1
#define		ST_FULL			2
#define		ST_GAME			3
#define		ST_WIN_X		4
#define		ST_DRAW			5
#define		ST_WIN_0		6

#define		CMD_JOIN		1
#define		CMD_STEP		2
#define		CMD_DRAW		3
#define		ANS_JOIN		4
#define		ANS_STEP		5
#define		ANS_DRAW		6
#define		ANS_END			7
#define		ANS_START		8
#define		CMD_GET_FIELD		9
#define		CMD_LOOSE		10
#define		ANS_FIELD		11
#define		ANS_OSTEP		12

#define 	P_DONE			100
#define		P_FAILED		101
#define		P_VALID			102
#define		P_NOT_VALID		103
#define		P_YES			104
#define		P_NO			105
#define		P_WIN			106
#define		P_LOOSE			107
#define		P_LOOSE_TIME		108
#define		P_XPLAYER		109
#define		P_OPLAYER		110
#define		P_DRAW

#endif 
