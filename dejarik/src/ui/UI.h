#ifndef UI_H_
#define UI_H_

#include <GL/gl.h>
#include <GL/glu.h>

#include "../include/Macros.h"
#include "Game.h"

CLASSPTR(UI);


class UI
{
public:
    
    UI(const GamePtr& game): mGame(game), mQuit(false)
    {}
    
    bool startup();
    
    void waitForEvents();
    
private:
    
    bool stop(bool res);
    bool resizeWindow(unsigned width, unsigned height);
    bool initGL();
    
    enum Color
    {
        CL_WHITE,
        CL_BLACK
    };
    
    bool drawAll();
    void drawBoard();
    void drawCell(Color color, float x1, float x2, float a1);
    
    void mouseCoordinatesToGL(float winX, float winY, GLdouble& posX, GLdouble& posY, GLdouble& posZ);

    
    GamePtr mGame; 
    bool mQuit;
    
};


#endif /*UI_H_*/
