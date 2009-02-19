#ifndef UI_H_
#define UI_H_

#include <GL/gl.h>
#include <GL/glu.h>
#include "SDL.h"

#include "../common/Macros.h"
#include "Game.h"

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
    bool resizeWindow(unsigned width, unsigned height);
    bool initGL();
    
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
    
    void mouseToGL(float winX, float winY, GLdouble& x, GLdouble& y, GLdouble& z);

    /*
     * Defines if the mouse click is valid (true/false).
     * if true, returns the clicked cell
     */
    bool isCellClicked(GLdouble x, GLdouble y,  CellPtr& cell);
    
    void onMouseClick(const SDL_Event& event);
    
    GamePtr mGame; 
    bool mQuit;
    
};


#endif /*UI_H_*/
