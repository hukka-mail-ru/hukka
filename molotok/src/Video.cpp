#include <iostream>
#include "Video.h"
#include "System.h"
#include "BMP.h"

using namespace std;


    
// virtual scene size
#define SCREEN_WIDTH  800 // must be big enough
#define SCREEN_HEIGHT 1000 // must be big enough
    
  
void Video::startup()
{
    TRY_BEGINS;
    
    initSDL();
    
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();   
    
    // The center of coordinates 
    glViewport(-(SCREEN_WIDTH - WINDOW_WIDTH)/2,
               -(SCREEN_HEIGHT - WINDOW_HEIGHT)/2,
               SCREEN_WIDTH, 
               SCREEN_HEIGHT);

    glOrtho(-SCREEN_WIDTH/2,  SCREEN_WIDTH/2,
             -SCREEN_HEIGHT/2, SCREEN_HEIGHT/2,
             0 , 1);

    glMatrixMode(GL_MODELVIEW);
    glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);
    

    // 2D rendering
    glDisable(GL_DEPTH_TEST);
    glDisable(GL_FOG);
    glDisable(GL_LIGHTING);
    glDisable(GL_ALPHA_TEST);
    glDisable(GL_DITHER);

    // Load all the textures 
    createTextures();
    
    TRY_RETHROW;
}

/* function to release/destroy our resources and restoring the old desktop */
void Video::stop()
{
    /* clean up the window */
    SDL_Quit();
}

void Video::initSDL()
{
    TRY_BEGINS;
    
    /* initialize SDL */
    if ( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
        stop();
        throw(runtime_error("Video initialization failed"));
    }

    /* Fetch the video info */
    const SDL_VideoInfo* videoInfo = SDL_GetVideoInfo( );

    if ( !videoInfo )
    {
        stop();
        throw(runtime_error("Video query failed"));
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

    /* get a SDL surface */
    SDL_Surface* surface = SDL_SetVideoMode( WINDOW_WIDTH, WINDOW_HEIGHT, SCREEN_BPP,
                videoFlags );

    /* Verify there is a surface */
    if ( !surface )
    {
        stop();
        throw(runtime_error("Video mode set failed"));
    }
    
    TRY_RETHROW;
}






void Video::drawSolidPolygon(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color)
{
    TRY_BEGINS;
    
    glLoadIdentity();

    
    glColor4f(
        color.r, 
        color.b, 
        color.g, 
        color.a);

    
    glEnableClientState(GL_VERTEX_ARRAY);
    glVertexPointer(3, GL_SHORT, 0, vertexArray);
    
    glDrawArrays(GL_TRIANGLE_FAN, 0, vertNum);
  //  glDrawElements(GL_TRIANGLE_FAN,vertNum, GL_UNSIGNED_BYTE, indices);


    glDisableClientState(GL_VERTEX_ARRAY);
    
    TRY_RETHROW;
}


void Video::drawLineLoop(const GLshort* vertexArray, unsigned vertNum, const RGBA_Color& color,
        float width)
{
    TRY_BEGINS;
    
    glLoadIdentity();
    
    glColor4f(
        color.r, 
        color.b, 
        color.g, 
        color.a);
    
    glLineWidth(width);

    glEnableClientState(GL_VERTEX_ARRAY);

    glVertexPointer(3, GL_SHORT, 0, vertexArray);
    
    glDrawArrays(GL_LINE_LOOP, 0, vertNum);

    glDisableClientState(GL_VERTEX_ARRAY);
    
    TRY_RETHROW;
}

void Video::drawSprite(
        const std::string& texName, const RGBA_Color& color, 
        BindXY bindXY, GLshort x, GLshort y, float angle)
{
    TRY_BEGINS;
    
    
    
    if(!textures[texName])
    {
        ostringstream os;
        os << "Can't find name " << texName;
        throw runtime_error(os.str()); 
    }
    
    TexturePtr texture = textures[texName];
    
    if(bindXY == XY_CENTER)
    {
        x -= texture->w / 2;
        y -= texture->h / 2;
    }
    else if(bindXY == XY_RIGHT_BOTTOM)
    {
        x -= texture->w;
    }
    else if(bindXY == XY_LEFT_TOP)
    {
        y -= texture->h;
    }
    else if(bindXY == XY_RIGHT_TOP)
    {
        x -= texture->w;
        y -= texture->h;
    }

                  
    glLoadIdentity();
    glEnable( GL_TEXTURE_2D );
    glBindTexture(GL_TEXTURE_2D, texture->id);
    
    glColor4f(
        color.r, 
        color.b, 
        color.g, 
        color.a);

    GLshort x1 = x;
    GLshort y1 = y;
    
    GLshort x2 = x  + texture->w;
    GLshort y2 = y + texture->h;

    glTranslatef((x1+x2)/2, 
                 (y1+y2)/2, 
                 0); // rotate [move to the coordinate center]
                 
    glRotatef(angle ,0, 0, 1); // rotation
    
    glTranslatef(-(x1+x2)/2, 
                 -(y1+y2)/2, 
                 0); // move back to the old position

    const GLshort vertices []=
    {
        x1,  y1, 0,
        x2,  y1, 0,
        x2,  y2, 0,
        x1,  y2, 0,
    };

    const GLshort texCoords[] = 
    {
        0, 0,
        1, 0,
        1, 1,
        0, 1,
    };

    glVertexPointer(3, GL_SHORT, 0, vertices);
    glTexCoordPointer(2, GL_SHORT, 0, texCoords);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY); 

        glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);

    glDisable( GL_TEXTURE_2D );

    TRY_RETHROW;
}


void Video::enableBlend()
{
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}


void Video::disableBlend()
{
    glDisable(GL_BLEND);
}



void Video::createTexture(const char* dir, const char* name)
{
    TRY_BEGINS;
    
    TexturePtr texture (new Texture); 
    
    ostringstream path;
    path << dir << "/" << name << ".bmp";
    
    /* Status indicator */
    bool res = false;

    /* Create storage space for the texture */
    EDR_SurfacePtr surface = EDR_LoadBMP(path.str().c_str()); 

    /* Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit */
    if (surface)
    {

        /* Set the status to true */
        res = true;

        /* Create The Texture */
        glGenTextures(1, &texture->id);

        /* Typical Texture Generation Using Data From The Bitmap */
        glBindTexture(GL_TEXTURE_2D, texture->id);

        /* Generate The Texture */
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, surface->w, surface->h, 0, GL_RGBA, // blue chanel must be changed by red 
                     GL_UNSIGNED_BYTE, surface->pixels );
        
        texture->w = surface->w;
        texture->h = surface->h;

        /* Linear Filtering */
        glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameterf( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
    }
    

    GLenum err = glGetError();
    if(!res || err != GL_NO_ERROR)
    {
        ostringstream os;
        os << "Error " << err << "; Path: " << path.str() << endl;
        throw runtime_error(os.str());
    }
    
    textures[name] = texture;  
    
    TRY_RETHROW;
}





void Video::createTextures()
{
    TRY_BEGINS;
      
   
    createTexture("img", "molotok");
    createTexture("img", "tile");

    
    TRY_RETHROW;
}
