#ifndef VIDEO_H_
#define VIDEO_H_

#include <vector>
#include <string>
#include <map>

#include <GLES/gl.h>

#include "../system/Macros.h"
#include "Window.h"

#define BOARD_TEXTURE_WIDTH 128

#define WINDOW_WIDTH  240
#define WINDOW_HEIGHT 320


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

    enum Blended
    {
        BLENDED_ON,
        BLENDED_OFF
    };

    struct Texture
    {
        unsigned id;
        GLfloat w;
        GLfloat h;
        Blended blended;
    };



    typedef boost::shared_ptr<Texture> TexturePtr; 
    
public:
    
    void startup(const std::vector<std::string>& pieceNames);
    
    void drawSolidPolygon(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color);
    
    void drawLineLoop(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color,
                      float width);

    void drawSprite(const std::string& textureName, const RGBA_Color& color, 
                    BindXY bindXY, GLshort x, GLshort y, float angle);    
        
private:

   
    void createTextures(const std::vector<std::string>& names);
    
    void createTexture(const char* dir, const char* name, Blended blended = BLENDED_OFF);
    void createCompressedTexture(const char* dir, const char* name, Blended blended = BLENDED_OFF);

    std::map<std::string, TexturePtr> textures;
};

#endif /*VIDEO_H_*/
