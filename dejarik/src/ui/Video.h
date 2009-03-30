#ifndef VIDEO_H_
#define VIDEO_H_

//#ifdef WIN_BUILD
//#include <windows.h>
//#endif

#include <vector>
#include <string>
#include <map>

#include <GLES/gl.h>

//#include "Glbasic.h"

//#ifndef OPENGL_ES_BUILD // no integration of SDL and OpenGL ES at the moment
//    #include <SDL.h>
//#endif


#include "../common/Macros.h"
#include "Window.h"

/*#define USE_FLOAT*/
#ifdef USE_FLOAT
#define glF(x)  x
#define glD(x)  x
#define GL_F    GL_FLOAT
typedef GLfloat GLf;
#else
#define glF(x)  ((GLfixed)((x)*(1<<16)))
#define glD(x)  glF(x)
#define GL_F    GL_FIXED
typedef GLfixed GLf;

#define glClearColor    glClearColorx
#define glTranslatef    glTranslatex
#define glRotatef   glRotatex
#define glMaterialfv    glMaterialxv
#define glMaterialf glMaterialx
#define glOrthof    glOrthox
#define glScalef    glScalex
#endif

/////////////////////////////////////////////////////////////////////////
#ifdef LINUX_BUILD
    #include <X11/Xmd.h> // for INT16, INT32
    #pragma pack(1)
    struct BITMAPFILEHEADER 
    {
    
        INT16 bfType;
        INT32 bfSize;
        INT16 bfReserved1;
        INT16 bfReserved2;
        INT32 bfOffBits;
    };
    
    #pragma pack(1)
    struct BITMAPINFOHEADER 
    { 
      INT32 biSize; 
      INT32 biWidth; 
      INT32 biHeight; 
      INT16 biPlanes; 
      INT16 biBitCount; 
      INT32 biCompression; 
      INT32 biSizeImage; 
      INT32 biXPelsPerMeter; 
      INT32 biYPelsPerMeter; 
      INT32 biClrUsed; 
      INT32 biClrImportant; 
    }; 
    
    #pragma pack(1)
    struct RGBTRIPLE 
    {
      INT8 rgbtRed;
      INT8 rgbtGreen;
      INT8 rgbtBlue;
    };
#endif
/////////////////////////////////////////////////////////////////////////

struct Texture
{
    unsigned id;
    float w;
    float h;
};

enum ImageType
{
    IT_SINGLE,
    IT_MASKED
};

struct Image
{
    ImageType type;
    Texture texture;
    Texture mask;
};

struct Surface
{
    unsigned char* pixels;
    int w;
    int h;
};

CLASSPTR(Image);


struct RGB_Color
{
    RGB_Color(float r, float g, float b): r(r), g(g), b(b) {}
    float r;
    float g;
    float b;
};

enum SpriteXY
{
    XY_CENTER,
    XY_LEFTBOTTOM
};


class Video
{
    
public:
    void startup(const std::vector<std::string>& pieceNames);
    void stop();
    
    
    void drawBackground();
    
    void drawPolygon(const std::vector<float>& x, const std::vector<float>& y, 
            const RGB_Color& color, float opacity);
     
    void drawShape(const std::vector<float>& xWin, const std::vector<float>& yWin, 
            const RGB_Color& color, float width);

    void drawSprite(const std::string& imageName, const RGB_Color& color, 
            SpriteXY spriteXY, float x, float y, float angle);    
        
private:
    
    Surface* loadBMP(const char* filename);
    void freeSurface(Surface* surface);
    
    void initSDL();
    void initGL();
    void createImages(const std::vector<std::string>& names);
    void createImage(const std::string& name, ImageType type);

    void loadTexture(Texture& texture, const std::string& path);
    void resizeWindow(unsigned width, unsigned height);
   
    void winToGL(float winX, float winY, float& x, float& y, float& z);
    
    void drawImage(const Texture& texture, const RGB_Color& color, float x, float y, float angle);    

    std::map<std::string, ImagePtr> images;
    
    int viewport[4]; 

};

#endif /*VIDEO_H_*/
