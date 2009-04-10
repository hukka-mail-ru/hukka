#pragma once
//#include "resourceppc.h"

#include <windows.h> //needed include for window system calls
#include <string>
#include <vector>

//OpenGL ES Includes
#include <GLES/gl.h>

/*EGL is the "machine" that glues OpenGL ES with the underlying
windowing system. We need it to create a suitable context and a drawable window*/
#include <GLES/egl.h>



/////// Window

extern HINSTANCE EDR_Instance;
extern int EDR_CmdShow;

enum EDR_EventType
{
    EVENT_LEFTMOUSEBUTTONDOWN,
    EVENT_RIGHTMOUSEBUTTONDOWN,
    EVENT_QUIT
};

struct EDR_Button
{
    int x;
    int y;
};

struct EDR_Event
{
    EDR_EventType type;
    EDR_Button button;
};

void EDR_CreateWindow(int width, int height,  const char *name);
void EDR_SwapBuffers(void); 

///////// Video 

struct RGBA_Color
{
    RGBA_Color(float r, float g, float b, float a): r(r), g(g), b(b), a(a) {}
    float r;
    float g;
    float b;
    float a;
};


class Video
{
public:

    void startup();

    void drawSolidPolygon(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color);
      
    void quitGLES();

};


class UI
{
    Video mVideo;
public:

    void startup();

    void drawAll();  // Our Render function
    
    void handleEvents();

};