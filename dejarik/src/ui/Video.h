#ifndef VIDEO_H_
#define VIDEO_H_

#include <GL/gl.h>
#include <GL/glu.h>
#include <SDL.h>


struct Texture
{
    unsigned id;
    float w;
    float h;
};

struct MaskedTexture
{
    Texture texture;
    Texture mask;
};

struct RGB
{
    RGB(unsigned r, unsigned g, unsigned b): r(r), g(g), b(b) {}
    unsigned r;
    unsigned g;
    unsigned b;
};


class Video
{
    
public:
    static bool startup();
    static bool stop(bool res);
   
    
    
    static void winToGL(float winX, float winY, GLdouble& x, GLdouble& y, GLdouble& z);
    static bool loadAllTextures();
    
    static void drawBackground();
    static void drawSprite(const Texture& texture, const RGB& color, float x, float y, float angle);    
    static void drawMaskedSprite(const MaskedTexture& mtex, const RGB& color, float x, float y, float angle);
    

    static Texture texture_bg; 
    static Texture board;    

    static MaskedTexture segment0;
    static MaskedTexture segment1;
    static MaskedTexture segment2;
    
private:
    static bool initGL();
    static bool loadTexture(Texture& texture, const char* path);
    static bool resizeWindow(unsigned width, unsigned height);
   
    

    
};

#endif /*VIDEO_H_*/
