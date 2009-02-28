#include <iostream>
#include "Video.h"


using namespace std;

/* screen width, height, and bit depth */
#define SCREEN_WIDTH  240
#define SCREEN_HEIGHT 320
#define SCREEN_BPP     16


Texture Video::texture_bg; /* Storage For One Texture ( NEW ) */
Texture Video::board; /* Storage For One Texture ( NEW ) */
Texture Video::piece; /* Storage For One Texture ( NEW ) */

MaskedTexture Video::segment0;
MaskedTexture Video::segment1;
MaskedTexture Video::segment2;

void Video::startup()
{
    TRY_BEGINS;
    
    /* initialize SDL */
    if ( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
        SDL_Quit();
        throw ("Video initialization failed");
    }

    /* Fetch the video info */
    const SDL_VideoInfo* videoInfo = SDL_GetVideoInfo( );
    if ( !videoInfo )
    {
        SDL_Quit();
        throw ("Video query failed");
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
        SDL_Quit();
        throw ("Video mode set failed");
    }

    /* initialize OpenGL */
    initGL();

    /* resize the initial window */
    resizeWindow( SCREEN_WIDTH, SCREEN_HEIGHT );

    
    TRY_RETHROW;
}

void Video::stop()
{
    TRY_BEGINS;
    SDL_Quit();
    TRY_RETHROW;
}


void Video::initGL()
{
    TRY_BEGINS;
        
    /* Load in the texture */
    loadAllTextures();

    /* Enable Texture Mapping ( NEW ) */
   // glEnable( GL_TEXTURE_2D );
    
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
 
    TRY_RETHROW;
}


void Video::resizeWindow(unsigned width, unsigned height)
{
    TRY_BEGINS;
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
   
    TRY_RETHROW;
}


void Video::winToGL(float winX, float winY, GLdouble& x, GLdouble& y, GLdouble& z)
{
    TRY_BEGINS;
    
    GLint viewport[4];                  // Where The Viewport Values Will Be Stored
    glGetIntegerv(GL_VIEWPORT, viewport); // Retrieves The Viewport Values (X, Y, Width, Height)
    
    GLdouble modelview[16];                 // Where The 16 Doubles Of The Modelview Matrix Are To Be Stored
    glGetDoublev(GL_MODELVIEW_MATRIX, modelview);       // Retrieve The Modelview Matrix

    GLdouble projection[16];                // Where The 16 Doubles Of The Projection Matrix Are To Be Stored
    glGetDoublev(GL_PROJECTION_MATRIX, projection);     // Retrieve The Projection Matrix

    winY = (float)viewport[3] - winY;           // Subtract The Current Mouse Y Coordinate From The Screen Height
    
    GLfloat winZ = 0;
    glReadPixels(winX, winY, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, &winZ);
    
    gluUnProject( winX, winY, winZ, modelview, projection, viewport, &x, &y, &z);
    
    TRY_RETHROW;
}



void Video::loadTexture(Texture& texture, const char* path)
{
    TRY_BEGINS;
    
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
        glGenTextures(1, &texture.id);

        /* Typical Texture Generation Using Data From The Bitmap */
        glBindTexture(GL_TEXTURE_2D, texture.id);

        /* Generate The Texture */
        glTexImage2D(GL_TEXTURE_2D, 0, 3, image->w, image->h, 0, GL_BGR,
                GL_UNSIGNED_BYTE, image->pixels );
        
        texture.w = image->w;
        texture.h = image->h;

        /* Linear Filtering */
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
        }

    /* Free up any memory we may have used */
    if (image)
        SDL_FreeSurface(image);

    if(!res)
    {
        ostringstream os;
        os << "loadTexture failed: " << path << endl;
        throw os.str();
    }
    
    TRY_RETHROW;
}

void Video::loadAllTextures()
{
    TRY_BEGINS;
    
    Video::loadTexture(segment0.texture, "img/segment0.bmp");    
    Video::loadTexture(segment0.mask, "img/segment0_mask.bmp");
    
    Video::loadTexture(segment1.texture, "img/segment.bmp");    
    Video::loadTexture(segment1.mask, "img/segment_mask.bmp");
    
    Video::loadTexture(segment2.texture, "img/segment2.bmp");    
    Video::loadTexture(segment2.mask, "img/segment2_mask.bmp");
    
    Video::loadTexture(board, "img/board.bmp");
    Video::loadTexture(piece, "img/piece.bmp");
    Video::loadTexture(texture_bg, "img/bg.bmp");
    
    TRY_RETHROW;
}

void Video::drawBackground()
{
    TRY_BEGINS;
    
    float x = -40;
    float y = -40; 
    float w = 80; 
    float h = 80;
    glBindTexture( GL_TEXTURE_2D, texture_bg.id);

    glBegin(GL_POLYGON);
      glTexCoord2f( 0, 1 ); glVertex3f(  x + 0,  y + 0, 0.0 );
      glTexCoord2f( 1, 1 ); glVertex3f(  x + w,  y + 0, 0.0 );
      glTexCoord2f( 1, 0 ); glVertex3f(  x + w,  y + h, 0.0 );
      glTexCoord2f( 0, 0 ); glVertex3f(  x + 0,  y + h, 0.0 );
    glEnd( ); 
 
    TRY_RETHROW;
  //  Video::drawSprite(Video::texture_bg, -5, -5, 10, 10); // just a big white sprite 
}


void Video::drawShape(const vector<float>& xWin, const vector<float>& yWin, const RGB& color, float width)
{
    TRY_BEGINS;
    
    vector<float> x; 
    vector<float> y;
    
    for(unsigned i=0; i< xWin.size(); i++)
    {
        GLdouble x1 = 0;
        GLdouble y1 = 0;
        GLdouble z1 = 0;
        Video::winToGL(xWin[i], yWin[i], x1, y1, z1);
        
        x.push_back( x1 );
        y.push_back( y1 );
    }
    
    
    glColor3f(color.r, color.g, color.b);
    glLineWidth(width);
    
        glBegin(GL_LINE_LOOP);

            for(unsigned i=0; i< x.size(); i++)
            {                
                glVertex3f( x[i], y[i], 0 );
            }
        glEnd( ); 
        
    glColor3f(1, 1, 1); // reset
    
    
    TRY_RETHROW;
}

void Video::drawPolygon(const vector<float>& xWin, const vector<float>& yWin, const RGB& color, float opacity)
{
    TRY_BEGINS;
    
    vector<float> x; 
    vector<float> y;
    
    for(unsigned i=0; i< xWin.size(); i++)
    {
        GLdouble x1 = 0;
        GLdouble y1 = 0;
        GLdouble z1 = 0;
        Video::winToGL(xWin[i], yWin[i], x1, y1, z1);
        
        x.push_back( x1 );
        y.push_back( y1 );
    }
    
    
    
    glEnable( GL_BLEND );   
    glDisable( GL_DEPTH_TEST );
   // glBlendFunc(GL_SRC_ALPHA, GL_ONE);
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); 
    
    glColor4f(color.r, color.g, color.b, opacity);
        
        glBegin(GL_POLYGON);

            for(unsigned i=0; i< x.size(); i++)
            {                
                glVertex3f( x[i], y[i], 0 );
            }
        glEnd( ); 
        
    glColor3f(1, 1, 1); // reset
    glEnable( GL_DEPTH_TEST ); /* Enable Depth Testing */
    glDisable( GL_BLEND );     /* Disable Blending     */
    
    
    TRY_RETHROW;
}

void Video::drawSprite(const Texture& texture, const RGB& color, float winX, float winY, float angle)
{
    TRY_BEGINS;
   
    
    GLdouble x1 = 0;
    GLdouble y1 = 0;
    GLdouble z1 = 0;
    Video::winToGL(winX, winY, x1, y1, z1);

    GLdouble x2 = 0;
    GLdouble y2 = 0;
    GLdouble z2 = 0;
    Video::winToGL(winX + texture.w, winY + texture.h, x2, y2, z2);
    
    glEnable( GL_TEXTURE_2D );
    glPushMatrix();
    
        glBindTexture( GL_TEXTURE_2D, texture.id);
        glColor3f(color.r, color.g, color.b); // blue
        glRotatef(angle ,0, 0, 1); // rotate
        
        
        glBegin(GL_POLYGON);
          glTexCoord2f( 0, 0 ); glVertex3f(  x1,  y1, 0.0 );
          glTexCoord2f( 1, 0 ); glVertex3f(  x2,  y1, 0.0 );
          glTexCoord2f( 1, 1 ); glVertex3f(  x2,  y2, 0.0 );
          glTexCoord2f( 0, 1 ); glVertex3f(  x1,  y2, 0.0 );
        glEnd( ); 
        
        glColor3f(1, 1, 1); // reset
    glPopMatrix();
    glDisable( GL_TEXTURE_2D );
    
    TRY_RETHROW;
}


void Video::drawMaskedSprite(const MaskedTexture& mtex, const RGB& color, float x, float y, float angle)
{
    TRY_BEGINS;
    
    glEnable( GL_BLEND );   
    glDisable( GL_DEPTH_TEST );
    glBlendFunc( GL_DST_COLOR, GL_ZERO );
    
    drawSprite(mtex.mask, RGB(1,1,1), x, y, angle);

    glBlendFunc( GL_ONE, GL_ONE );

    
    drawSprite(mtex.texture, color, x, y, angle);
    
    glEnable( GL_DEPTH_TEST ); /* Enable Depth Testing */
    glDisable( GL_BLEND );     /* Disable Blending     */

    TRY_RETHROW;
}
