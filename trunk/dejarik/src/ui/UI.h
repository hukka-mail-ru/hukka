#ifndef UI_H_
#define UI_H_


#include "../common/Macros.h"
#include "Game.h"

#include <GL/gl.h>
#include <GL/glu.h>
#include <SDL.h>

CLASSPTR(UI);


class UI
{
public:
    
    UI(const GamePtr& game): mGame(game), mQuit(false)
    {}
    
    bool startup();
    
    void handleEvents();
    
private:
    
    bool stop(bool res);
    
   
    
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
    void drawCell(const CellPtr& cell);
    void drawPiece(const CellPtr& cell);
    void drawActivePlayer();
    
    void drawSquare(); // temp
    void drawBg(); // temp
    
    

    /*
     * Defines if the mouse click is valid (true/false).
     * if true, returns the clicked cell
     */
    bool isCellClicked(float x, float y,  CellPtr& cell);
    
    void onMouseClick(const SDL_Event& event);
    
    GamePtr mGame; 
    bool mQuit;
    

};


#endif /*UI_H_*/
