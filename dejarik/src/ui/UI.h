#ifndef UI_H_
#define UI_H_


#include "../common/Macros.h"
#include "Game.h"
#include "Animation.h"
#include "Video.h"

CLASSPTR(UI);


class UI
{
public:
    
    UI(const GamePtr& game):
        mGame(game), mQuit(false), animation(game), menuItemName("default"), mMoving(false)
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
    void drawCell(const CellPtr& cell, bool clicked);
    void drawPiece(const PiecePtr& piece);
    void drawActivePlayer();
    void drawMenu();
    float getFinishAngle(unsigned step, const CellPtr& cell);
    
    /*
     * Defines if the mouse click is valid (true/false).
     * if true, returns the clicked cell
     */
    bool isCellClicked(float x, float y,  CellPtr& cell);
    
    void onMouseClick(int button_x, int button_y);
    
    GamePtr mGame; 
    bool mQuit;    
    std::vector<CellPtr> mMoveSteps; 
    
    Animation animation;
    std::string menuItemName;
    bool mMoving;

    Video mVideo;
};


#endif /*UI_H_*/
