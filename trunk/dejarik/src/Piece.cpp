#include "Piece.h"

unsigned Piece::getAttackRating()
{
    return mAttackRating;
}

unsigned Piece::getDefenceRating()
{
    return mDefenceRating;
}

unsigned Piece::getMoveRating()
{
    return mMoveRating;
}

CellPtr& Piece::getPosition()
{
    return mPosition;
}

void Piece::setPosition(const CellPtr& cell)
{
    mPosition = cell;
}

PlayerPtr& Piece::getPlayer()
{
    return mPlayer;
}

void Piece::setPlayer(const PlayerPtr& player)
{
    mPlayer = player;
}
