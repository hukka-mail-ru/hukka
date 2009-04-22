#ifndef UI_H_
#define UI_H_


#include "Macros.h"
#include "Video.h"

CLASSPTR(UI);


class UI
{
public:
    
    UI(): mX(0), mY(0), mAngle(0) {}
    
    void startup();
    void stop();
    
    void handleEvents();
    
    
private:
    
    void drawCycle();
    bool drawAll();
    void onMouseClick(int x, int y);
    
    Video mVideo;
    
    short mX;
    short mY;
    float mAngle;
    

};


#endif /*UI_H_*/
