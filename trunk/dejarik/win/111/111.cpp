#include "111.h"
#include <windows.h>
#include <commctrl.h>


//Some useful global handles
HINSTANCE hInst; //Will hold the current instance of the application
HWND hWnd; // A handle to the window we will create
HDC hDC;   // A handle to the device context of the window. Needed to 
     //create the OpenGL ES context

// OpenGL variables
EGLDisplay glesDisplay;  // EGL display
EGLSurface glesSurface;	 // EGL rendering surface
EGLContext glesContext;	 // EGL rendering context

TCHAR szAppName[] = L"OpenGLES"; /*The application name and the window caption*/
 
/*This is the WinMain function. Here we will create the rendering window, initialize OpenGL ES, write the message loop, and, at the end, clean all and release all used resources*/
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine,	int nCmdShow)
{
  MSG msg; //This is the message variable for the message loop
  WNDCLASS	wc; /*This structure will hold some init values for our window*/
  hInst = hInstance; // Initialization of our global variable
  bool done = FALSE; 
	  
  /*This block of code is to ensure that the user only can run one
    instance of the application. First we search for a window with the 
    same class name, if found, we will focus it and return*/
  if(hWnd = FindWindow(szAppName, szAppName)) 
  {
    /* Set focus to foremost child window. The "| 0x01" is used to 
       bring any owned windows to the foreground and activate them.*/
    SetForegroundWindow((HWND)((ULONG) hWnd | 0x00000001));
    return 0;
  } 
		
  wc.style          = CS_HREDRAW | CS_VREDRAW; /*Will force a redraw 
  if the window is resized, both horizontally or vertically*/
  wc.lpfnWndProc    = (WNDPROC) WndProc; /*this is a function pointer,
  to tell the OS what function should call when a message needs to be 
  processed*/
  wc.cbClsExtra     = 0;
  wc.cbWndExtra     = 0;
  wc.hInstance      = hInstance;
  wc.hIcon          = LoadIcon(hInstance, NULL);//Load default icon
  wc.hCursor	      = 0; // Default cursor
  wc.hbrBackground  = 0; //We don't care about the background color
  wc.lpszMenuName	  = NULL; //This application does not have a menu
  wc.lpszClassName  = szAppName; /*Important, here we must fill the
   application class name (the class name is not the same than the 
   caption of the window, but many times they are the same)*/

  //Before creating the window, we must register this new window class
  if(!RegisterClass(&wc))
    return FALSE;
	
  hWnd=CreateWindow(szAppName, //Class Name
                    szAppName, //Caption string
                    WS_VISIBLE,//Window style
                    CW_USEDEFAULT,CW_USEDEFAULT,//Starting [x,y] pos.
                    CW_USEDEFAULT, CW_USEDEFAULT, //Width and height
                    NULL, NULL, //Parent window and menu handle
                    hInst, NULL); /*Instance handle. Custom value to 
                    pass in the creation with the WM_CREATE message*/
	                  
  if(!hWnd) return FALSE;
  if(!InitOGLES()) return FALSE; //OpenGL ES Initialization
  
  //Bring the window to front, focus it and refresh it
  ShowWindow(hWnd, nCmdShow); 
  UpdateWindow(hWnd);
 
  //Message Loop
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
	    Render();
  }
  //Clean up all
  Clean();
  return 0;
}
//----------------------------------------------------------------------------
LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam)
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
BOOL InitOGLES()
{  
  EGLConfig configs[10];
  EGLint matchingConfigs;	

  /*configAttribs is a integers list that holds the desired format of 
   our framebuffer. We will ask for a framebuffer with 24 bits of 
   color and 16 bits of z-buffer. We also ask for a window buffer, not 
   a pbuffer or pixmap buffer*/	
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
  
  hDC = GetWindowDC(hWnd);
  glesDisplay = eglGetDisplay(hDC);	 //Ask for an available display

  //Display initialization (we don't care about the OGLES version numbers)
  if(!eglInitialize(glesDisplay, NULL, NULL)) 
    return FALSE;
	
  /*Ask for the framebuffer confiburation that best fits our 
  parameters. At most, we want 10 configurations*/
  if(!eglChooseConfig(glesDisplay, configAttribs, &configs[0], 10,  &matchingConfigs)) 
   return FALSE;
	
  //If there isn't any configuration enough good
  if (matchingConfigs < 1)  return FALSE;	  

  /*eglCreateWindowSurface creates an onscreen EGLSurface and returns 
  a handle  to it. Any EGL rendering context created with a 
  compatible EGLConfig can be used to render into this surface.*/
  glesSurface = eglCreateWindowSurface(glesDisplay, configs[0], hWnd, configAttribs);	
  if(!glesSurface) return FALSE;
  
  // Let's create our rendering context
  glesContext=eglCreateContext(glesDisplay,configs[0],0,configAttribs);

  if(!glesContext) return FALSE;

  //Now we will activate the context for rendering	
  eglMakeCurrent(glesDisplay, glesSurface, glesSurface, glesContext); 
    
  /*Remember: because we are programming for a mobile device, we cant 
  use any of the OpenGL ES functions that finish in 'f', we must use 
  the fixed point version (they finish in 'x'*/
  glClearColorx(0, 0, 0, 0);
  glShadeModel(GL_SMOOTH);  
  /*In order to set a viewport that fits entirely our window, we need 
  to know the window dimensions. They could be obtained through the   
  WinCE call GetWindowRect, using our window handle*/
  RECT r;
  GetWindowRect(hWnd, &r);  
  glViewport(r.left, r.top, r.right - r.left, r.bottom - r.top);	
	
  /*Setup of the projection matrix. We will use an ortho cube centered 
  at (0,0,0) with 100 units of edge*/
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();    
  glOrthox(FixedFromInt(-50), FixedFromInt(50), 
           FixedFromInt(-50), FixedFromInt(50), 
           FixedFromInt(-50), FixedFromInt(50));
  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  return TRUE;
}
//----------------------------------------------------------------------------
void Render()
{
  static int rotation = 0;
  				  /* Vertex 1    Vertex 2 	 Vertex 3*/                           
  GLshort vertexArray[9] = {-25,-25,0,   25,-25,0,     0,25,0 };
  GLubyte colorArray[12] = {255,0,0,0,   0,255,0,0,    0,0,255,0};
  
  glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);  
  glLoadIdentity();  

  glTranslatex(0, 0, FixedFromInt(-10));
  glRotatex(FixedFromInt(rotation++), 0, ONE,0);  

  //Enable the vertices array  
  glEnableClientState(GL_VERTEX_ARRAY);
  glVertexPointer(3, GL_SHORT, 0, vertexArray);
  //3 = XYZ coordinates, GL_SHORT = data type, 0 = 0 stride bytes
	
  //Enable the vertex color array
  glEnableClientState(GL_COLOR_ARRAY);
  glColorPointer(4,GL_UNSIGNED_BYTE, 0, colorArray);
  //4 = RGBA format, GL_UNSIGNED_BYTE = data type,0=0 stide    bytes

  glDrawArrays(GL_TRIANGLES, 0, 3);
  /*We want draw triangles, 0 = first element(vertice), 3 = number of 
    items (vertices) to draw from the array*/  

  glDisableClientState(GL_VERTEX_ARRAY);
  glDisableClientState(GL_COLOR_ARRAY);
 	
  eglSwapBuffers(glesDisplay, glesSurface);
}
//----------------------------------------------------------------------------
void Clean()
{
  if(glesDisplay)
  {
    eglMakeCurrent(glesDisplay, NULL, NULL, NULL);  
    if(glesContext) eglDestroyContext(glesDisplay, glesContext);
    if(glesSurface) eglDestroySurface(glesDisplay, glesSurface);
    eglTerminate(glesDisplay);
  }
  //We have to destroy the window too
  DestroyWindow(hWnd);
  UnregisterClass(szAppName, hInst);  
}









