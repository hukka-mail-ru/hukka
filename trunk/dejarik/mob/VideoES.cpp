#include "VideoES.h"


using namespace std;

Texture texture1;
EGLDisplay egldisplay;
EGLSurface eglwindow;
EGLContext eglcontext;

#define SCREEN_WIDTH  800 // must be big enough
#define SCREEN_HEIGHT 1000 // must be big enough

#define WINDOW_WIDTH  240
#define WINDOW_HEIGHT 320

#define BOARD_TEXTURE_WIDTH 128

HINSTANCE hInstance;
int cmdShow;

// ====================================================================================
// ====================================================================================
// ====================================================================================

//----------------------------------------------------------------------------
// THIS FUNCTION IS A HELPER FOR VIDEO::initWin
LRESULT CALLBACK wndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
{
    switch (message) 
    {		
    case WM_PAINT:	
        ValidateRect(hWnd,NULL); //Needed to avoid new WM_PAINT messages
        return 0; 

    case WM_DESTROY:
        PostQuitMessage(0);
        return 0;  
    };
    return DefWindowProc(hWnd, message, wParam, lParam);   
}


//----------------------------------------------------------------------------
//HWND initWin(HINSTANCE hInstance, LPCWSTR szAppName, int cmdShow)
void EDR_CreateWindow(int width, int height,  const char *name)
{
    WNDCLASS	wc;
    
    LPCWSTR szAppName = L"Dejarik";
    HWND hWnd = FindWindow(szAppName, szAppName);
    if(hWnd) 
    {
        SetForegroundWindow((HWND)((ULONG) hWnd | 0x00000001));
        return;
    } 

    wc.style          = CS_HREDRAW | CS_VREDRAW; 
    wc.lpfnWndProc    = (WNDPROC) wndProc; 
    wc.cbClsExtra     = 0;
    wc.cbWndExtra     = 0;
    wc.hInstance      = hInstance;
    wc.hIcon          = LoadIcon(hInstance, NULL);//Load default icon
    wc.hCursor	      = 0; // Default cursor
    wc.hbrBackground  = 0; //We don't care about the background color
    wc.lpszMenuName	  = NULL; //This application does not have a menu
    wc.lpszClassName  = szAppName; 

    //Before creating the window, we must register this new window class
    if(!RegisterClass(&wc))
        return;

    hWnd=CreateWindow(szAppName, //Class Name
        szAppName, //Caption string
        WS_VISIBLE,//Window style
        CW_USEDEFAULT,CW_USEDEFAULT,//Starting [x,y] pos.
        CW_USEDEFAULT, CW_USEDEFAULT, //Width and height
        NULL, NULL, //Parent window and menu handle
        hInstance, NULL);

    if(!hWnd) return;

    //Bring the window to front, focus it and refresh it
    ShowWindow(hWnd, cmdShow); 
    UpdateWindow(hWnd);
    
    ///////////////////////////////////////////////////
    EGLConfig configs[10];
    EGLint matchingConfigs;	

    const EGLint configAttribs[] =
    {
        EGL_RED_SIZE,       8,
        EGL_GREEN_SIZE,     8,
        EGL_BLUE_SIZE,      8,
        EGL_ALPHA_SIZE,     EGL_DONT_CARE,
        EGL_DEPTH_SIZE,     16,
        EGL_STENCIL_SIZE,   EGL_DONT_CARE,
        EGL_SURFACE_TYPE,   EGL_WINDOW_BIT,
        EGL_NONE,           EGL_NONE
    };

    HDC hDC = GetWindowDC(hWnd);
    egldisplay = eglGetDisplay(hDC);	 //Ask for an available display

    if(!eglInitialize(egldisplay, NULL, NULL)) 
        return;

    if(!eglChooseConfig(egldisplay, configAttribs, &configs[0], 10,  &matchingConfigs)) 
        return;

    if (matchingConfigs < 1)  
        return;	  

    eglwindow = eglCreateWindowSurface(egldisplay, configs[0], hWnd, configAttribs);	
    if(!eglwindow) 
        return;

    eglcontext=eglCreateContext(egldisplay,configs[0],0,configAttribs);

    if(!eglcontext) 
        return;

    eglMakeCurrent(egldisplay, eglwindow, eglwindow, eglcontext); 

    return;
}




// ====================================================================================
// ====================================================================================
// ====================================================================================

void Video::startup()
{
    EDR_CreateWindow(WINDOW_WIDTH, WINDOW_HEIGHT, "Dejarik");

    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();   

    // The center of coordinates must be at the center of the game board  
    // (a litte upper of geometrical window center) 
    glViewport(-(SCREEN_WIDTH - WINDOW_WIDTH)/2,
        -(SCREEN_HEIGHT - WINDOW_HEIGHT)/2  + (WINDOW_HEIGHT/2 - BOARD_TEXTURE_WIDTH),
        SCREEN_WIDTH, 
        SCREEN_HEIGHT);

    glOrthox(FixedFromInt(-SCREEN_WIDTH/2),  FixedFromInt(SCREEN_WIDTH/2),
        FixedFromInt(-SCREEN_HEIGHT/2), FixedFromInt(SCREEN_HEIGHT/2),
        FixedFromInt(0) , FixedFromInt(1));

    glMatrixMode(GL_MODELVIEW);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

}


void EDR_SwapBuffers(void) 
{
    eglSwapBuffers(egldisplay, eglwindow);
}

void Video::drawSolidPolygon(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color)
{
    glLoadIdentity();

    glColor4x(
        FixedFromFloat(color.r), 
        FixedFromFloat(color.b), 
        FixedFromFloat(color.g), 
        FixedFromFloat(color.a));

    glEnableClientState(GL_VERTEX_ARRAY);

    glVertexPointer(3, GL_SHORT, 0, vertexArray);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

    glDrawArrays(GL_TRIANGLE_FAN, 0, vertNum);

    glDisable(GL_BLEND);
    glDisableClientState(GL_VERTEX_ARRAY);
}


//----------------------------------------------------------------------------
void Video::drawAll()
{

    GLshort vertex[] =
    {
        0,0,0,
        10,0,0,
        10,10,0,
        0,10,0
    };
    
    drawSolidPolygon(vertex, 4, RGBA_Color(1,1,1,1));
    
    EDR_SwapBuffers();
}





//----------------------------------------------------------------------------
void Video::quitGLES()
{
    

    if(egldisplay)
    {
        eglMakeCurrent(egldisplay, NULL, NULL, NULL);  
        if(eglcontext) eglDestroyContext(egldisplay, eglcontext);
        if(eglwindow) eglDestroySurface(egldisplay, eglwindow);
        eglTerminate(egldisplay);
    }
}

