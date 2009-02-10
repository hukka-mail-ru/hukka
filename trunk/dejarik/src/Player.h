#ifndef PLAYER_H_
#define PLAYER_H_

#include <string>
#include <vector>
#include "Macros.h"
#include "Piece.h"


CLASSPTR(Piece);

class Player
{
public:
    Player(): mName("default") {}
    
    void addPiece(const PiecePtr& piece)
    {
        mPieces.push_back(piece);
    }
    
    bool makeTurn(unsigned c, unsigned x, unsigned click)
    {
        return false;
    }
    
private:
    std::string mName;
    
    std::vector<PiecePtr> mPieces;
};

CLASSPTR(Player);


#endif /*PLAYER_H_*/
