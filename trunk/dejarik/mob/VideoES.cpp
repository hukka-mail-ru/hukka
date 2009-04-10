#include "VideoES.h"


using namespace std;

EGLDisplay egldisplay;
EGLSurface eglwindow;
EGLContext eglcontext;

#define SCREEN_WIDTH  800 // must be big enough
#define SCREEN_HEIGHT 1000 // must be big enough

#define WINDOW_WIDTH  240
#define WINDOW_HEIGHT 320

#define BOARD_TEXTURE_WIDTH 128


#define PRECISION 16	
#define ONE	(1 << PRECISION)
#define ZERO 0

#define MAXPATHLEN  1024
#define TEXTURE_FILTER_NONE 0
#define TEXTURE_FILTER_BILINEAR 1
#define TEXTURE_FILTER_TRILINEAR 2


inline GLfixed FixedFromInt(int value) {return value << PRECISION;}
inline GLfixed FixedFromFloat(float value) {return static_cast<GLfixed>(value * static_cast<float>(ONE));}
inline GLfixed MultiplyFixed(GLfixed op1, GLfixed op2) {return (op1 * op2) >> PRECISION;};


HINSTANCE EDR_Instance;
int EDR_CmdShow;

bool EDR_PollEvent(EDR_Event& event)
{ 
    for(;;)
    {
        MSG msg; //This is the message variable for the message loop
        if(PeekMessage(&msg,NULL,0,0,PM_REMOVE))
        {
            if(msg.message==WM_QUIT)
            {
                event.type = EVENT_QUIT;
                return true;
            }
            else
            { 
                TranslateMessage(&msg);
                DispatchMessage(&msg);
            }
        }
    }

}

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
    wc.lpfnWndProc    = (WNDPROC)DefWindowProc;
    wc.cbClsExtra     = 0;
    wc.cbWndExtra     = 0;
    wc.hInstance      = EDR_Instance;
    wc.hIcon          = LoadIcon(EDR_Instance, NULL);//Load default icon
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
        EDR_Instance, NULL);

    if(!hWnd) return;

    //Bring the window to front, focus it and refresh it
    ShowWindow(hWnd, EDR_CmdShow); 
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

void EDR_SwapBuffers(void) 
{
    eglSwapBuffers(egldisplay, eglwindow);
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

//----------------------------------------------------------------------------
void UI::drawAll()
{

    GLshort vertex[] =
    {
        0,0,0,
        10,0,0,
        10,10,0,
        0,10,0
    };
    
    mVideo.drawSolidPolygon(vertex, 4, RGBA_Color(1,1,1,1));
    
    EDR_SwapBuffers();
}


void UI::startup()
{
    mVideo.startup();
}

void UI::handleEvents()
{
    for(;;)
    {

        drawAll();

        /* handle the events in the queue */        
        EDR_Event event;
        if(EDR_PollEvent(event))
        {
            if(event.type == EVENT_QUIT) 
            {
                break;
            }
        }
    }
}




