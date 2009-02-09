#ifndef PLAYER_H_
#define PLAYER_H_

#include <string>
#include "Macros.h"

class Player
{
public:
    Player(): mName("default") {}
    
private:
    std::string mName;
};

CLASSPTR(Player);


#endif /*PLAYER_H_*/
