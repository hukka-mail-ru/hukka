#ifndef UI_H_
#define UI_H_


#include "../system/Macros.h"
#include "Game.h"
#include "Animation.h"
#include "Video.h"

CLASSPTR(UI);


class UI
{
public:
    
    UI(const GamePtr& game):
        mGame(game), animation(game), menuItemName("default"), mMoving(false)
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
    
    bool drawField();
    void drawBoard();
    void drawCell(const CellPtr& cell, bool clicked);
    void drawPiece(const PiecePtr& piece);
    //void drawActivePlayer();
    void drawMenu();
    
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
    std::vector<CellPtr> mMoveSteps; 
    
    Animation animation;
    std::string menuItemName;
    bool mMoving;
    PiecePtr mActivePiece;

    Video mVideo;
};


#endif /*UI_H_*/
