#include "Piece.h"

unsigned Piece::getAttackRating() const
{
    return mAttackRating;
}

unsigned Piece::getDefenceRating() const
{
    return mDefenceRating;
}

unsigned Piece::getMoveRating() const
{
    return mMoveRating;
}

const CellPtr& Piece::getPosition() const
{
    return mPosition;
}

const std::string Piece::getName() const
{
    return mName;
}


void Piece::setPosition(const CellPtr& cell)
{
    mPosition = cell;
}


const Player* Piece::getPlayer() const
{
    return mPlayer;
}

void Piece::setPlayer(Player* player)
{
    mPlayer = player;
}


