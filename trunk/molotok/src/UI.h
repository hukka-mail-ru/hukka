#ifndef UI_H_
#define UI_H_


#include "Macros.h"
#include "Video.h"

CLASSPTR(UI);


class UI
{
public:
    
    void startup();
    
    void handleEvents();
    
private:
    

    bool drawAll();
    void onMouseClick(int x, int y);
    
    Video mVideo;
};


#endif /*UI_H_*/
