
#include "VideoES.h"
//#include "Glbasic.h"


#define SCREEN_WIDTH  240
#define SCREEN_HEIGHT 320
using namespace std;


UI ui;


/*This is the WinMain function. Here we will create the rendering window, initialize OpenGL ES, write the message loop, and, at the end, clean all and release all used resources*/
int WINAPI WinMain(HINSTANCE inst, HINSTANCE hPrevInstance, LPTSTR lpCmdLine,	int cmd)
{   
    hInstance = inst;
    cmdShow = cmd;
    
    ui.startup();
        

    //Message Loop
    ui.handleEvents();
    
    //Clean up all
  //  video.quitGLES();
    
    //We have to destroy the window too
//    DestroyWindow(hWnd);
 //   UnregisterClass(szAppName, hInstance);  
     
    return 0;
}









