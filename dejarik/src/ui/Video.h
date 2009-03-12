#ifndef VIDEO_H_
#define VIDEO_H_

#ifdef WIN_BUILD
#include <windows.h>
#endif

#include <vector>
#include <string>
#include <map>
#include <GL/gl.h>
#include <GL/glu.h>
#include <SDL.h>
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

struct RGB
{
    RGB(float r, float g, float b): r(r), g(g), b(b) {}
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
    static void startup(const std::vector<std::string>& pieceNames);
    static void stop();
    
    
    static void drawBackground();
    
    static void drawPolygon(const std::vector<float>& x, const std::vector<float>& y, 
            const RGB& color, float opacity);
    
    static void drawShape(const std::vector<float>& xWin, const std::vector<float>& yWin, 
            const RGB& color, float width);

    static void drawSprite(const std::string& imageName, const RGB& color, 
            SpriteXY spriteXY, float x, float y, float angle);    
        
private:
    
    static void initGL();
    static void createImages(const std::vector<std::string>& names);
    static void createImage(const std::string& name, ImageType type);

    static void loadTexture(Texture& texture, const std::string& path);
    static void resizeWindow(unsigned width, unsigned height);
   
    static void winToGL(float winX, float winY, GLdouble& x, GLdouble& y, GLdouble& z);
    
    static void drawImage(const Texture& texture, const RGB& color, float x, float y, float angle);    

    static std::map<std::string, ImagePtr> images;

};

#endif /*VIDEO_H_*/
