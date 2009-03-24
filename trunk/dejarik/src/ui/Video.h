#ifndef VIDEO_H_
#define VIDEO_H_

//#ifdef WIN_BUILD
//#include <windows.h>
//#endif

#include <vector>
#include <string>
#include <map>

#include "Glbasic.h"

//#ifndef OPENGL_ES_BUILD // no integration of SDL and OpenGL ES at the moment
    #include <SDL.h>
//#endif

#include "../common/Macros.h"


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
