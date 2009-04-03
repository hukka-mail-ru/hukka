#ifndef VIDEO_H_
#define VIDEO_H_

#include <vector>
#include <string>
#include <map>

#include <GLES/gl.h>

#include "../system/Macros.h"
#include "Window.h"

#define BOARD_TEXTURE_WIDTH 128

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
    
    struct Surface // for BMP loading
    {
        unsigned char* pixels;
        int w;
        int h;
    };

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
    
public:
    
    void startup(const std::vector<std::string>& pieceNames);
    void stop();
    
    // a new version
    void drawPolygon(GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color);
    
    void drawShape(const std::vector<float>& xWin, const std::vector<float>& yWin, 
            const RGBA_Color& color, float width);

    void drawSprite(const std::string& imageName, const RGBA_Color& color, 
            BindXY bindXY, GLshort x, GLshort y, float angle);    
        
private:
    
    Surface* loadBMP(const char* filename);
    void freeSurface(Surface* surface);

    // a new version
    void drawImage(const Texture& texture, const RGBA_Color& color, 
                   float winX, float winY, float angle);
    
    void createImages(const std::vector<std::string>& names);
    void createImage(const std::string& name, ImageType type);

    void loadTexture(Texture& texture, const std::string& path);
    void resizeWindow(unsigned width, unsigned height);
   
    void winToGL(float winX, float winY, float& x, float& y, float& z);
    


    std::map<std::string, ImagePtr> images;
    
    int viewport[4]; 

};

#endif /*VIDEO_H_*/
