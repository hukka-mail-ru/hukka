#ifndef VIDEO_H_
#define VIDEO_H_

//#ifdef WIN_BUILD
//#include <windows.h>
//#endif

#include <vector>
#include <string>
#include <map>

#include <GLES/gl.h>


#define PRECISION 16    
#define ONE (1 << PRECISION)
#define ZERO 0

#define MAXPATHLEN  1024
#define TEXTURE_FILTER_NONE 0
#define TEXTURE_FILTER_BILINEAR 1
#define TEXTURE_FILTER_TRILINEAR 2

inline GLfixed FixedFromInt(int value) {return value << PRECISION;}
inline GLfixed FixedFromFloat(float value) {return static_cast<GLfixed>(value * static_cast<float>(ONE));}
inline GLfixed MultiplyFixed(GLfixed op1, GLfixed op2) {return (op1 * op2) >> PRECISION;};


#include "../common/Macros.h"
#include "Window.h"

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


struct RGBA_Color
{
    RGBA_Color(float r, float g, float b, float a): r(r), g(g), b(b), a(a) {}
    float r;
    float g;
    float b;
    float a;
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
    
    // a new version
    void drawPolygon(float* vertexArray, unsigned vertNum, const RGBA_Color& color);
    // a new version
    void drawImage(const std::string& imageName, const RGBA_Color& color, 
                   float winX, float winY, float angle);
     
    void drawShape(const std::vector<float>& xWin, const std::vector<float>& yWin, 
            const RGB_Color& color, float width);

    void drawSprite(const std::string& imageName, const RGB_Color& color, 
            SpriteXY spriteXY, float x, float y, float angle);    
        
private:
    
    Surface* loadBMP(const char* filename);
    void freeSurface(Surface* surface);
    
    void setPerspective(GLfloat fovy, GLfloat aspect, GLfloat zNear,  GLfloat zFar);
    
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
