#ifndef VIDEO_H_
#define VIDEO_H_

#include <GL/gl.h>
#include <GL/glu.h>
#include <SDL.h>


struct Texture
{
    GLuint id;
    float w;
    float h;
};

struct MaskedTexture
{
    Texture texture;
    Texture mask;
};

class Video
{
    
public:
    static bool startup();
    static bool stop(bool res);
   
    
    
    static void winToGL(float winX, float winY, GLdouble& x, GLdouble& y, GLdouble& z);
    static bool loadAllTextures();
    
    static void drawBackground();
    static void drawSprite(const Texture& texture, float x, float y);    
    static void drawMaskedSprite(const MaskedTexture& mtex, float x, float y);
    

    static Texture texture_bg; /* Storage For One Texture ( NEW ) */    
    static MaskedTexture mtex1;
    
private:
    static bool initGL();
    static bool loadTexture(Texture& texture, const char* path);
    static bool resizeWindow(unsigned width, unsigned height);
   
    

    
};

#endif /*VIDEO_H_*/
