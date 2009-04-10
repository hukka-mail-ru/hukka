
#include "VideoES.h"
//#include "Glbasic.h"


#define SCREEN_WIDTH  240
#define SCREEN_HEIGHT 320
using namespace std;


Video video;

void handleEvents()
{
    MSG msg; //This is the message variable for the message loop
    bool done = FALSE; 
    while(!done)
    {
        if(PeekMessage(&msg,NULL,0,0,PM_REMOVE))
        {
            if(msg.message==WM_QUIT)
                done=TRUE;
            else
            { 
                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
        }
        else
        {
            video.drawAll();
        }
    }

}

/*This is the WinMain function. Here we will create the rendering window, initialize OpenGL ES, write the message loop, and, at the end, clean all and release all used resources*/
int WINAPI WinMain(HINSTANCE inst, HINSTANCE hPrevInstance, LPTSTR lpCmdLine,	int cmd)
{   
    hInstance = inst;
    cmdShow = cmd;
    
    video.startup();
        
        
        
    video.startup();

    //Message Loop
    handleEvents();
    
    //Clean up all
    video.quitGLES();
    
    //We have to destroy the window too
//    DestroyWindow(hWnd);
 //   UnregisterClass(szAppName, hInstance);  
     
    return 0;
}









