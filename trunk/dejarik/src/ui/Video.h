#ifndef VIDEO_H_
#define VIDEO_H_

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
    static void startup();
    static void stop();
    
    static void loadAllTextures();
    
    static void drawBackground();
    static void drawSprite(const Texture& texture, const RGB& color, float x, float y, float angle);    
    static void drawMaskedSprite(const MaskedTexture& mtex, const RGB& color, float x, float y, float angle);
    

    static Texture texture_bg; 
    static Texture board;
    static Texture piece;   

    static MaskedTexture segment0;
    static MaskedTexture segment1;
    static MaskedTexture segment2;
    
private:
    static void initGL();
    static void loadTexture(Texture& texture, const char* path);
    static void resizeWindow(unsigned width, unsigned height);
   
    static void winToGL(float winX, float winY, GLdouble& x, GLdouble& y, GLdouble& z);
    

    
};

#endif /*VIDEO_H_*/
