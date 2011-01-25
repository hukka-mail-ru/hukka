#ifndef ICHESSLOGIC_H_
#define ICHESSLOGIC_H_

#include <interface.h>
#include "ChessGameStatus.h"

/**
* @file IChessLogic.h
* @author Alexey Karelin
* @class IGameLogic
* @brief Interface for chess logic classes
*/

class IChessLogic: public IGameLogic
{
    /**
     * @brief Return status of current game
     * @return ChessGameStatus
     */
    virtual ChessGameStatus GetStatus() = 0;
};


#endif /*ICHESSLOGIC_H_*/
