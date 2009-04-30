#ifndef UI_H_
#define UI_H_


#include "../system/Macros.h"
#include "Game.h"
#include "Animation.h"
#include "Video.h"

CLASSPTR(UI);


class UI
{
    friend class TestAnimation;
    
public:
    
    UI(const GamePtr& game):
        mGame(game), mAnimation(game), mMoving(false)
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
    
    void drawAll();
    void drawField();
    void drawBoard();
    void drawMenu();
    void drawCell(const CellPtr& cell, bool clicked);
    void drawPiece(const PiecePtr& piece);
    //void drawActivePlayer();
   
    void getActiveFields(const float x, const float y, unsigned& first, unsigned& second);
    
    void memorizeField();
    
    void createCell(const CellPtr cell);
    void createCellSegment(const CellPtr cell, float radius1, float radius2);
    /*
     * Defines if the mouse click is valid (true/false).
     * if true, returns the clicked cell
     */
    bool isCellClicked(int x, int y,  CellPtr& cell);    
    void onMouseClick(int x, int y);
    
    GamePtr mGame;    
    Animation mAnimation;
    Video mVideo;
    
    bool mMoving;
    std::vector<CellPtr> mMoveSteps; 
    PiecePtr mActivePiece;

};


#endif /*UI_H_*/
