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

using namespace std;

static int attributeList[] = { EGL_RED_SIZE, 1, EGL_DEPTH_SIZE, 1, EGL_NONE };

#ifdef _WIN32
void
EDR_CreateWindow(int width, int height,  const char *name) {
    WNDCLASS	wc;
    DWORD	dwExStyle;
    DWORD	dwStyle;
    RECT	WindowRect;
    HWND        hwnd;
    HDC         hdc;
    HINSTANCE   hInstance;
    EGLConfig config[4];
    EGLContext cx;
    int nconfig;

    WindowRect.left = (long)0;
    WindowRect.right = (long)width;
    WindowRect.top = (long)0;
    WindowRect.bottom = (long)height;

    hInstance		= GetModuleHandle(NULL);
    wc.style		= CS_HREDRAW | CS_VREDRAW | CS_OWNDC;
    wc.lpfnWndProc	= (WNDPROC)DefWindowProc;
    wc.cbClsExtra	= 0;
    wc.cbWndExtra	= 0;
    wc.hInstance	= hInstance;
    wc.hIcon		= LoadIcon(NULL, IDI_WINLOGO);
    wc.hCursor		= LoadCursor(NULL, IDC_ARROW);
    wc.hbrBackground	= NULL;
    wc.lpszMenuName	= NULL;
    wc.lpszClassName	= "OpenGL";

    if (!RegisterClass(&wc)) {
	printf("Failed To Register The Window Class.\n");
	exit(1);
    }
	

    dwExStyle = WS_EX_APPWINDOW | WS_EX_WINDOWEDGE;
    dwStyle = WS_OVERLAPPEDWINDOW;

    AdjustWindowRectEx(&WindowRect, dwStyle, FALSE, dwExStyle);

    if (!(hwnd = CreateWindowEx(dwExStyle, "OpenGL", name,
		dwStyle | WS_CLIPSIBLINGS | WS_CLIPCHILDREN, 0, 0,
		WindowRect.right-WindowRect.left,
		WindowRect.bottom-WindowRect.top, NULL, NULL, hInstance, NULL))) {
	//DestroyWin();
	printf("Window Creation Error.\n");
	exit(1);
    }

    if (!(hdc = GetDC(hwnd))) {
	//DestroyWin();
	printf("Can't Create A GL Device Context.\n");
	exit(1);
    }

    egldisplay = eglGetDisplay(hdc);
    eglInitialize(egldisplay, NULL, NULL);
    if (!eglChooseConfig(egldisplay, attributeList, config, sizeof config/sizeof config[0], &nconfig)) {
	printf("can't find requested config\n");
	exit(1);
    }

    eglwindow = eglCreateWindowSurface(egldisplay, config[0], (NativeWindowType)hwnd, 0);
    cx = eglCreateContext(egldisplay, config[0], 0, 0);
    eglMakeCurrent(egldisplay, eglwindow, eglwindow, cx);

    ShowWindow(hwnd, SW_SHOW);
    SetForegroundWindow(hwnd);
    SetFocus(hwnd);
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
                    case Button1: event.type = EVENT_LEFTMOUSEBUTTONDOWN; break;
                    case Button3: event.type = EVENT_RIGHTMOUSEBUTTONDOWN; break;
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
    
