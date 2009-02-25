#include <iostream>
#include "Video.h"


using namespace std;

/* screen width, height, and bit depth */
#define SCREEN_WIDTH  240
#define SCREEN_HEIGHT 320
#define SCREEN_BPP     16


GLuint Video::texture_sprite; /* Storage For One Texture ( NEW ) */
GLuint Video::texture_mask; /* Storage For One Texture ( NEW ) */
GLuint Video::texture_bg; /* Storage For One Texture ( NEW ) */

bool Video::startup()
{
    /* initialize SDL */
    if ( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
        cerr << "Video initialization failed: " <<  SDL_GetError( ) << endl;
        return Video::stop(false);
    }

    /* Fetch the video info */
    const SDL_VideoInfo* videoInfo = SDL_GetVideoInfo( );
    if ( !videoInfo )
    {
        cerr << "Video query failed: " <<  SDL_GetError( ) << endl;
        return stop(false);
    }

    /* the flags to pass to SDL_SetVideoMode */
    int videoFlags  = SDL_OPENGL;          /* Enable OpenGL in SDL */
    videoFlags |= SDL_GL_DOUBLEBUFFER; /* Enable double buffering */
    videoFlags |= SDL_HWPALETTE;       /* Store the palette in hardware */
    videoFlags |= SDL_RESIZABLE;       /* Enable window resizing */

    /* This checks to see if surfaces can be stored in memory */
    if ( videoInfo->hw_available )
        videoFlags |= SDL_HWSURFACE;
    else
        videoFlags |= SDL_SWSURFACE;

    /* This checks if hardware blits can be done */
    if ( videoInfo->blit_hw )
        videoFlags |= SDL_HWACCEL;

    /* Sets up OpenGL double buffering */
    SDL_GL_SetAttribute( SDL_GL_DOUBLEBUFFER, 1 );

    /* Verify there is a surface */
    if (!SDL_SetVideoMode( SCREEN_WIDTH, SCREEN_HEIGHT, SCREEN_BPP, videoFlags ) )
    {
        cerr <<  "Video mode set failed: " << SDL_GetError( ) << endl;
        return stop(false);
    }

    /* initialize OpenGL */
    initGL( );

    /* resize the initial window */
    resizeWindow( SCREEN_WIDTH, SCREEN_HEIGHT );

    return true;
}

bool Video::stop(bool res)
{
    SDL_Quit();
    return res;
}

bool Video::resizeWindow(unsigned width, unsigned height)
{
    /* Protect against a divide by zero */
    if ( height == 0 )
        height = 1;

    /* Height / width ration */
    GLfloat ratio = ( GLfloat )width / ( GLfloat )height;

    /* Setup our viewport. */
    glViewport( 0, 0, ( GLsizei )width, ( GLsizei )height );

    /* change to the projection matrix and set our viewing volume. */
    glMatrixMode( GL_PROJECTION );
    glLoadIdentity( );

    /* Set our perspective */
    gluPerspective( 45.0f, ratio, 0.1f, 100.0f );

    /* Make sure we're chaning the model view and not the projection */
    glMatrixMode( GL_MODELVIEW );

    /* Reset The View */
    glLoadIdentity( );

    return true;    
}


bool Video::initGL()
{
    /* Load in the texture */
    if (!loadAllTextures())
        return false;

    /* Enable Texture Mapping ( NEW ) */
    glEnable( GL_TEXTURE_2D );
    
    /* Enable smooth shading */
    glShadeModel( GL_SMOOTH );

    /* Set the background grey */
    glClearColor( 0.5f, 0.5f, 0.5f, 0.0f );

    /* Depth buffer setup */
    glClearDepth( 1.0f );

    /* Enables Depth Testing */
    glEnable( GL_DEPTH_TEST );

    /* The Type Of Depth Test To Do */
    glDepthFunc( GL_LEQUAL );

    /* Really Nice Perspective Calculations */
    glHint( GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST );

    return true;    
}

void Video::mouseToGL(float winX, float winY, GLdouble& x, GLdouble& y, GLdouble& z)
{
    GLint viewport[4];                  // Where The Viewport Values Will Be Stored
    glGetIntegerv(GL_VIEWPORT, viewport); // Retrieves The Viewport Values (X, Y, Width, Height)
    
    GLdouble modelview[16];                 // Where The 16 Doubles Of The Modelview Matrix Are To Be Stored
    glGetDoublev(GL_MODELVIEW_MATRIX, modelview);       // Retrieve The Modelview Matrix

    GLdouble projection[16];                // Where The 16 Doubles Of The Projection Matrix Are To Be Stored
    glGetDoublev(GL_PROJECTION_MATRIX, projection);     // Retrieve The Projection Matrix

    winY = (float)viewport[3] - winY;           // Subtract The Current Mouse Y Coordinate From The Screen Height
    
    GLfloat winZ;
    glReadPixels(winX, winY, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, &winZ);
    
    gluUnProject( winX, winY, winZ, modelview, projection, viewport, &x, &y, &z);
}



bool Video::loadTexture(GLuint& texture, const char* path)
{
    /* Status indicator */
    bool res = false;

    /* Create storage space for the texture */
    SDL_Surface* image; 

    /* Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit */
    if ((image = SDL_LoadBMP(path)))
        {

        /* Set the status to true */
        res = true;

        /* Create The Texture */
        glGenTextures(1, &texture);

        /* Typical Texture Generation Using Data From The Bitmap */
        glBindTexture(GL_TEXTURE_2D, texture);

        /* Generate The Texture */
        glTexImage2D(GL_TEXTURE_2D, 0, 3, image->w, image->h, 0, GL_BGR,
                GL_UNSIGNED_BYTE, image->pixels );

        /* Linear Filtering */
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
        }

    /* Free up any memory we may have used */
    if (image)
        SDL_FreeSurface(image);

    return res;    
}

bool Video::loadAllTextures()
{
    if(!Video::loadTexture(texture_sprite, "img/sprite1.bmp"))
        return false;
    
    if(!Video::loadTexture(texture_sprite, "img/mask1.bmp"))
        return false;
    
    if(!Video::loadTexture(texture_bg, "img/bg.bmp"))
        return false;
    
    return true;
}
