/*
 * GLESonGL implementation
 * Version:  1.0
 *
 * Copyright (C) 2003  David Blythe   All Rights Reserved.
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
 * DAVID BLYTHE BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN
 * AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include "stdlib.h"
#include "stdio.h"
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN 1
#include <windows.h>
#else
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <X11/keysym.h>
#endif

#include <GLES/egl.h>
#include "Window.h"
#include "System.h"
#include "../system/Macros.h"


EGLSurface eglwindow;
EGLDisplay egldisplay;
EGLContext eglcontext;

using namespace std;

static int attributeList[] = { EGL_RED_SIZE, 1, EGL_DEPTH_SIZE, 1, EGL_NONE };

#ifdef _WIN32

HINSTANCE EDR_Instance;
int EDR_CmdShow;
HWND EDR_HWnd;

bool EDR_PollEvent(EDR_Event& event)
{ 
    MSG msg; //This is the message variable for the message loop
    if(PeekMessage(&msg,NULL,0,0,PM_NOREMOVE))
    {
        while( GetMessage( &msg, NULL, 0, 0 ) )
        {
            switch(msg.message)
            {
            case WM_QUIT:
                event.type = EVENT_QUIT;
                return true;
            case WM_LBUTTONDOWN:
                event.type = EVENT_LBUTTONDOWN;
                event.button.x = LOWORD(msg.lParam);
                event.button.y = HIWORD(msg.lParam);
                return true;
            case WM_RBUTTONDOWN:
                event.type = EVENT_RBUTTONDOWN;
                event.button.x = LOWORD(msg.lParam);
                event.button.y = HIWORD(msg.lParam);
                return true;
            default:
                TranslateMessage(&msg);
                DispatchMessage(&msg);
                return true;
            }
        }
    }
    return false;
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

#else

Display* display;
Window window;

Atom pnProtocol;
Atom nWMProtocols;


bool EDR_PollEvent(EDR_Event& event)
{  

    while (XPending(display))
    {

        XEvent xEvent;
        XNextEvent(display, &xEvent);
        
        switch(xEvent.type)
        {
            case ClientMessage:
                if(xEvent.xclient.message_type == nWMProtocols && 
                   xEvent.xclient.data.l[0] == (long)pnProtocol)
                {
                    event.type = EVENT_QUIT;
                    return true;
                }
                break;
            
            case ButtonPress:
                switch(xEvent.xbutton.button)        
                {
                    case Button1: event.type = EVENT_LBUTTONDOWN; break;
                    case Button3: event.type = EVENT_RBUTTONDOWN; break;
                    default: break;
                }
                
                event.button.x = xEvent.xbutton.x;
                event.button.y = xEvent.xbutton.y;
                return true;
                
            default:
                break;
        }
    }

    return false;
}

void EDR_CreateWindow(int width, int height, const char *name) 
{  
    TRY_BEGINS;
        
    EGLConfig config[4];
    EGLContext cx;
    int nconfig;
    XSizeHints sizehints;
    XSetWindowAttributes swa;
    XVisualInfo *vi, tmp;
    int vid, n;


    display = XOpenDisplay(NULL);
    egldisplay = eglGetDisplay(display);
    
    eglInitialize(egldisplay, 0, 0);
    
    if (!eglChooseConfig(egldisplay, attributeList, config, sizeof config/sizeof config[0], &nconfig)) 
    {
        throw runtime_error("Can't find requested config");
    }
    
    cx = eglCreateContext(egldisplay, config[0], 0, 0);

    eglGetConfigAttrib(egldisplay, config[0], EGL_NATIVE_VISUAL_ID, &vid);
    tmp.visualid = vid;
    vi = XGetVisualInfo(display, VisualIDMask, &tmp, &n);
    swa.colormap = XCreateColormap(display, RootWindow(display, vi->screen),
                           vi->visual, AllocNone);
    sizehints.flags = 0;
    sizehints.flags = PMinSize | PMaxSize;
    sizehints.min_width = sizehints.max_width = width;
    sizehints.min_height = sizehints.max_height = height;


    swa.border_pixel = 0;
    swa.event_mask = ExposureMask | StructureNotifyMask | KeyPressMask | ButtonPressMask | ButtonReleaseMask;
    window = XCreateWindow(display, RootWindow(display, vi->screen), 0, 0, width, height,
                        0, vi->depth, InputOutput, vi->visual,
                        CWBorderPixel|CWColormap|CWEventMask, &swa);
    

    // this is for correct handling of the 'close' operation
    pnProtocol = XInternAtom (display, "WM_DELETE_WINDOW", True);
    nWMProtocols = XInternAtom (display, "WM_PROTOCOLS", True);    
    XSetWMProtocols (display, window, &pnProtocol, 1);

    
    // show window
    XMapWindow(display, window);
    XSetStandardProperties(display, window, name, name, None, 0, 0, &sizehints);

    eglwindow = eglCreateWindowSurface(egldisplay, config[0], (NativeWindowType)window, 0);
    eglMakeCurrent(egldisplay, eglwindow, eglwindow, cx);

    TRY_RETHROW;
}



#endif




void EDR_SwapBuffers(void) 
{
    eglSwapBuffers(egldisplay, eglwindow);
}
    
