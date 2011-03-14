/*
 * Player.h
 *
 *  Created on: Mar 14, 2011
 *      Author: ssy
 */

#ifndef PLAYER_H_
#define PLAYER_H_

#include <QString>
#include <Defines.h>

struct Player
{
    QString     name;
    int         rating;
    PlayerColor color;
    bool        isAuthorized;
};

#endif /* PLAYER_H_ */
