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

//int WINAPI WinMain(HINSTANCE hInst, HINSTANCE hPrevInst, LPTSTR lpCmdLine,int nCmdShow);
//LRESULT CALLBACK WndProc(HWND hWnd, UINT message, WPARAM wParam, LPARAM lParam);

struct RGBA_Color
{
    RGBA_Color(float r, float g, float b, float a): r(r), g(g), b(b), a(a) {}
    float r;
    float g;
    float b;
    float a;
};


struct Texture
{
    GLuint id;
    int w;
    int h;
};

extern HINSTANCE hInstance;
extern int cmdShow;

class Video
{
public:

  //  bool initGLES(HWND hWnd);// Our GL initialization function
    void quitGLES();


    ////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////
    void drawAll();  // Our Render function

    void drawSolidPolygon(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color);

    void startup();

private:


    void drawImage(const Texture& texture, const RGBA_Color& color, 
                   float winX, float winY, float angle);
        
    void drawPolygon(float* vertexArray, unsigned vertNum, const RGBA_Color& color);   
    int  loadTexture(LPCWSTR filename, Texture& texture); 
    void setPerspective(HWND hWnd);
    void perspective(GLfloat fovy, GLfloat aspect, GLfloat zNear,  GLfloat zFar);

    std::wstring getCurDir();
      
    // OpenGL variables
  //  EGLDisplay mGlesDisplay;     // EGL display
 //   EGLSurface mGlesSurface;	 // EGL rendering surface
 //   EGLContext mGlesContext;	 // EGL rendering context
    ////////////////////////////////////////////////////////////////////
    ////////////////////////////////////////////////////////////////////
    int viewport[4]; 
};