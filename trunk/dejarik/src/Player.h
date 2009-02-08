#ifndef PLAYER_H_
#define PLAYER_H_

#include <string>
#include <boost/shared_ptr.hpp> 

class Player
{
public:
    Player(): mName("default") {}
    
private:
    std::string mName;
};

typedef boost::shared_ptr<Player> PlayerPtr;


#endif /*PLAYER_H_*/
