#ifndef VIDEO_H_
#define VIDEO_H_

#include <vector>
#include <string>
#include <map>

#include <GL/gl.h>
#include <SDL/SDL.h>

#include "Macros.h"

#define BOARD_TEXTURE_WIDTH 128


#define WINDOW_WIDTH  640
#define WINDOW_HEIGHT 480
#define SCREEN_BPP     16


struct RGBA_Color
{
    RGBA_Color(float r, float g, float b, float a): r(r), g(g), b(b), a(a) {}
    float r;
    float g;
    float b;
    float a;
};


enum BindXY
{
    XY_CENTER,
    XY_LEFT_BOTTOM,
    XY_RIGHT_BOTTOM,
    XY_LEFT_TOP,
    XY_RIGHT_TOP
};



class Video
{
private:
    
    struct BMPSurface // for BMP loading
    {
        GLbyte* pixels;
        int w;
        int h;
    };

    struct Texture
    {
        unsigned id;
        GLfloat w;
        GLfloat h;
    };



    typedef boost::shared_ptr<Texture> TexturePtr; 
    
public:
    
    void startup();
    void stop();
    
    void drawSolidPolygon(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color);
    
    void drawLineLoop(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color,
                      float width);

    void drawSprite(const std::string& textureName, const RGBA_Color& color, 
                    BindXY bindXY, GLshort x, GLshort y, float angle);    
    
    void enableBlend();
    void disableBlend();
        
private:

   
    void createTextures();
    
    void createTexture(const char* dir, const char* name);
    void createCompressedTexture(const char* dir, const char* name);

    std::map<std::string, TexturePtr> textures;
};

#endif /*VIDEO_H_*/
