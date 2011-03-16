
#ifndef TBMDEFS_H
#define TBMDEFS_H

// TABLE MANAGER 50 -
const int CMD_CREATE         = 51;
const int CMD_RANDOM_OP      = 52;
const int CMD_GETMYTBL       = 53;
const int CMD_FIND           = 54;
const int CMD_GET_PARAMS     = 55;
const int CMD_SET_PARAMS     = 56;
const int CMD_DELETE         = 57;
const int ANS_CREATE	     = 58;
const int ANS_TABLE		     = 59;
const int ANS_MYTBL		     = 60;
const int ANS_RANDOM_OP      = 61;
const int ANS_GET_PARAMS     = 62;
const int ANS_SET_PARAMS     = 63;
const int ANS_DELETE         = 64;

const int ST_NOTVALID          = 11;
const int ST_NOTVALID_LOGICID  = 12;
const int ST_VALID             = 13;
const int ST_VALID_AND_OWNER   = 14;


#endif
