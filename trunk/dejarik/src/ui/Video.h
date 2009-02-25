#ifndef VIDEO_H_
#define VIDEO_H_

#include <GL/gl.h>
#include <GL/glu.h>
#include <SDL.h>

struct MaskedTexture
{
    GLuint texture;
    GLuint mask;
};

class Video
{
    
public:
    static bool startup();
    static bool stop(bool res);
   
    
    static void mouseToGL(float winX, float winY, GLdouble& x, GLdouble& y, GLdouble& z);
    static bool loadAllTextures();
    
    static void drawSprite(GLuint texture, float x, float y, float w, float h);    
    static void drawMaskedSprite(const MaskedTexture& mtex, float x, float y, float w, float h);
    

    static GLuint texture_bg; /* Storage For One Texture ( NEW ) */    
    static MaskedTexture mtex1;
    
private:
    static bool initGL();
    static bool loadTexture(GLuint& texture, const char* path);
    static bool resizeWindow(unsigned width, unsigned height);
    

    
};

#endif /*VIDEO_H_*/
