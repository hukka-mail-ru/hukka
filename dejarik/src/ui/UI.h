#ifndef UI_H_
#define UI_H_


#include "../common/Macros.h"
#include "Game.h"
#include "CellImage.h"
#include "PieceImage.h"
#include "Animation.h"

#include <GL/gl.h>
#include <GL/glu.h>
#include <SDL.h>
#include <map>

CLASSPTR(UI);


class UI
{
public:
    
    UI(const GamePtr& game): mGame(game), mQuit(false), mMoving(false)
    {}
    
    void startup();
    
    void handleEvents();
    

private:
    
    enum Color
    {
        CL_WHITE,
        CL_BLACK,
        CL_GREEN,
        CL_BLUE,
        CL_RED
    };
    
    bool drawAll();
    void drawBoard();
    void drawCell(const CellImagePtr& cell, bool clicked);
    void drawPiece(const PieceImagePtr& piece);
    void drawActivePlayer();
    
    /*
     * Defines if the mouse click is valid (true/false).
     * if true, returns the clicked cell
     */
    bool isCellClicked(float x, float y,  CellPtr& cell);
    
    void onMouseClick(const SDL_Event& event);
    
    GamePtr mGame; 
    bool mQuit;
    bool mMoving; // piece moving in progress
    std::vector<CellPtr> mMoveSteps; 
    
    std::map<CellPtr, CellImagePtr> mCellImages;
    std::map<PiecePtr, PieceImagePtr> mPieceImages;
    
    Animation animation; // 1 UI has 1 Animation

};


#endif /*UI_H_*/
