#include <iostream>
#include "Video.h"


using namespace std;

/* screen width, height, and bit depth */
#define SCREEN_WIDTH  240
#define SCREEN_HEIGHT 320
#define SCREEN_BPP     16


void Video::startup(const std::vector<std::string>& pieceNames)
{
    TRY_BEGINS;
    
    /* initialize SDL */
    initSDL();

    /* Load all the textures */
    createImages(pieceNames);
    
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

void Video::initSDL()
{
    TRY_BEGINS;

    /* initialize SDL */
    if ( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
        SDL_Quit();
        throw runtime_error("Video initialization failed");
    }

    /* Fetch the video info */
    const SDL_VideoInfo* videoInfo = SDL_GetVideoInfo( );
    if ( !videoInfo )
    {
        SDL_Quit();
        throw runtime_error("Video query failed");
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
        throw runtime_error("Video mode set failed");
    }


    TRY_RETHROW;
}

void Video::initGL()
{
    TRY_BEGINS;

    /* Enable smooth shading */
    glShadeModel( GL_SMOOTH );

    /* Set the background grey */
    glClearColorx( 0.5f, 0.5f, 0.5f, 0.0f );

    /* Depth buffer setup */
  //  glClearDepth( 1.0f ); // Can't work in ES. It seems we can just comment it

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
    viewport[0] = 0;
    viewport[1] = 0;
    viewport[2] = width;
    viewport[3] = height;
    glViewport(viewport[0], viewport[1], (GLsizei)viewport[2], (GLsizei)viewport[3] );

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


void Video::winToGL(float winX, float winY, float& x, float& y, float& z)
{
    TRY_BEGINS;

    // TODO Rrrrhh! The magic numbers should be somehow calculated, based on GL_PROJECTION and GL_MODELVIEW
    float modelview[16] = {1,0,0,0,0,1,0,0,0,0,1,0,0,0,-10,1} ;              
    float projection[16] = {3.218951,0,0,0,0,2.414213,0,0,0,0,-1.002002,-1,0,0,-0.2002002,0};

    
    winY = (float)viewport[3] - winY;           // Subtract The Current Mouse Y Coordinate From The Screen Height
    
    GLfloat winZ = 0.990991; // A magic number :( because opengl ES doesn't have GL_DEPTH_COMPONENT   
    // glReadPixels(winX, winY, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, &winZ);    
    
    gluUnProject( winX, winY, winZ, modelview, projection, viewport, &x, &y, &z);
    
    TRY_RETHROW;
}



void Video::loadTexture(Texture& texture, const std::string& path)
{
    TRY_BEGINS;
    
    /* Status indicator */
    bool res = false;

    /* Create storage space for the texture */
    SDL_Surface* image; 

    /* Load The Bitmap, Check For Errors, If Bitmap's Not Found Quit */
    if ((image = SDL_LoadBMP(path.c_str())))
        {

        /* Set the status to true */
        res = true;

        /* Create The Texture */
        glGenTextures(1, &texture.id);

        /* Typical Texture Generation Using Data From The Bitmap */
        glBindTexture(GL_TEXTURE_2D, texture.id);

        /* Generate The Texture */
        glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, image->w, image->h, 0, GL_RGB, // blue chanel must be changed by red 
                GL_UNSIGNED_BYTE, image->pixels );
        
        texture.w = image->w;
        texture.h = image->h;

        /* Linear Filtering */
        glTexParameterx( GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR );
        glTexParameterx( GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR );
        }

    /* Free up any memory we may have used */
    if (image)
        SDL_FreeSurface(image);

    if(!res)
    {
        ostringstream os;
        os << "loadTexture failed: " << path << endl;
        throw runtime_error(os.str());
    }
    
    TRY_RETHROW;
}




void Video::createImage(const std::string& name, ImageType type)
{
	TRY_BEGINS;

    ImagePtr image (new Image); 
    image->type = type;
    
    ostringstream fullname;
    fullname << "img/" << name << ".bmp";
    
    loadTexture(image->texture, fullname.str());  
    
    if(type == IT_MASKED)
    {
        ostringstream fullmask;
        fullmask << "img/" << name << "_mask.bmp";
        loadTexture(image->mask, fullmask.str());
    }
    
    images[name] = image;  

	TRY_RETHROW;
}

void Video::createImages(const std::vector<std::string>& names)
{
    TRY_BEGINS;
    
    for(unsigned i =0; i<names.size(); ++i)
    {
        createImage(names[i] +"0", IT_MASKED);        
        createImage("menu_" + names[i], IT_SINGLE);
    }
    
    
    for(unsigned i =1; i<8; ++i)
    {
        ostringstream name;
        name << "Molator" << i;
        createImage(name.str(), IT_MASKED);
    }
    
   
    createImage("menu_default", IT_SINGLE);
    createImage("menu_kill", IT_SINGLE);
    createImage("menu_push", IT_SINGLE);
    createImage("menu_counter_kill", IT_SINGLE);
    createImage("menu_counter_push", IT_SINGLE);
    
    createImage("segment0", IT_MASKED);
    createImage("segment", IT_MASKED);
    createImage("segment2", IT_MASKED);
        
    createImage("board", IT_SINGLE);
    createImage("piece", IT_SINGLE);
    createImage("bg", IT_SINGLE);

    TRY_RETHROW;
}

void Video::drawBackground()
{
    TRY_BEGINS;
    
    float x = -40;
    float y = -40; 
    float w = 80; 
    float h = 80;
  
    glBindTexture( GL_TEXTURE_2D, images["bg"]->texture.id);

    const GLbyte vertices []=
    {
        x + 0,  y + 0, 0,
        x + w,  y + 0, 0,
        x + w,  y + h, 0,
        x + 0,  y + h, 0
    };
     
    
    glVertexPointer(3, GL_FLOAT, 0, vertices);
    glTexCoordPointer(2, GL_FLOAT, 0, vertices);
    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);

    TRY_RETHROW;
}


void Video::drawShape(const vector<float>& xWin, const vector<float>& yWin, const RGB_Color& color, float width)
{
    TRY_BEGINS;
    
    vector<float> vertices; 
    
    for(unsigned i=0; i< xWin.size(); i++)
    {
        float x = 0;
        float y = 0;
        float z = 0;
        Video::winToGL(xWin[i], yWin[i], x, y, z);
        
        vertices.push_back( x );
        vertices.push_back( y );
        vertices.push_back( 0 );
    }
    
    glColor4x(color.r, color.g, color.b, 0);
    glLineWidthx(width);
    
    
    glVertexPointer(3, GL_FLOAT, 0, &vertices[0]);
    glEnableClientState(GL_VERTEX_ARRAY);

        glDrawArrays(GL_LINE_LOOP, 0, xWin.size());

    glDisableClientState(GL_VERTEX_ARRAY);
 
    glColor4x(1, 1, 1, 0); // reset
    
    
    TRY_RETHROW;
}


void Video::drawPolygon(
        const vector<float>& xWin, const vector<float>& yWin, 
        const RGB_Color& color, float opacity)
{
    TRY_BEGINS;
    vector<float> vertices; 
    
    for(unsigned i=0; i< xWin.size(); i++)
    {
        float x = 0;
        float y = 0;
        float z = 0;
        Video::winToGL(xWin[i], yWin[i], x, y, z);
        
        vertices.push_back( x );
        vertices.push_back( y );
        vertices.push_back( 0 );
    }
    
    
    glEnable( GL_BLEND );   
    glDisable( GL_DEPTH_TEST );
    glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA); 
    
    glColor4x(color.r, color.g, color.b, opacity);
        
    glVertexPointer(3, GL_FLOAT, 0, &vertices[0]);
    glEnableClientState(GL_VERTEX_ARRAY);

        glDrawArrays(GL_TRIANGLE_FAN, 0, xWin.size());

    glDisableClientState(GL_VERTEX_ARRAY);
        
    glColor4x(1, 1, 1, 0); // reset
    glEnable( GL_DEPTH_TEST ); /* Enable Depth Testing */
    glDisable( GL_BLEND );     /* Disable Blending     */
    
    
    TRY_RETHROW;
}

void Video::drawSprite(
        const std::string& imageName, const RGB_Color& color, 
        SpriteXY spriteXY, float winX, float winY, float angle)
{
    TRY_BEGINS;
    
    if(!images[imageName])
    {
        ostringstream os;
        os << "Can't find name " << imageName;
        throw runtime_error(os.str()); 
    }
    
    if(spriteXY == XY_CENTER)
    {
        winX -= images[imageName]->texture.w / 2;
        winY -= images[imageName]->texture.h / 2;
    }
    
    
    if(images[imageName]->type == IT_SINGLE)
    {
        drawImage(images[imageName]->texture, color, winX, winY, angle);
    }
    else if(images[imageName]->type == IT_MASKED)
    {
        glEnable( GL_BLEND );   
        glDisable( GL_DEPTH_TEST );
        glBlendFunc( GL_DST_COLOR, GL_ZERO );
        
        drawImage(images[imageName]->mask, RGB_Color(1,1,1), winX, winY, angle);

        glBlendFunc( GL_ONE, GL_ONE );

        
        drawImage(images[imageName]->texture, color, winX, winY, angle);
        
        glEnable( GL_DEPTH_TEST ); /* Enable Depth Testing */
        glDisable( GL_BLEND );     /* Disable Blending     */
    }
    
    TRY_RETHROW;
}


void Video::drawImage(const Texture& texture, const RGB_Color& color, float winX, float winY, float angle)
{
    TRY_BEGINS;
   
    
    float x1 = 0;
    float y1 = 0;
    float z1 = 0;
    Video::winToGL(winX, winY, x1, y1, z1);

    float x2 = 0;
    float y2 = 0;
    float z2 = 0;
    Video::winToGL(winX + texture.w, winY + texture.h, x2, y2, z2);
    
    
    glPushMatrix();
    glEnable( GL_TEXTURE_2D );
    
        glBindTexture(GL_TEXTURE_2D, texture.id);
        glColor4x(color.r, color.g, color.b, 0); // blue
        
        glTranslatex((x1+x2)/2, (y1+y2)/2, 0); // rotate [move to the coordinate center]
        glRotatex(angle ,0, 0, 1); // rotation
        glTranslatex(-(x1+x2)/2, -(y1+y2)/2, 0); // move back to the old position
        
        const float vertices []=
        {
            x1,  y1, 0,
            x2,  y1, 0,
            x2,  y2, 0,
            x1,  y2, 0,
        };
         
        const float texCoords[] = 
        {
                0, 0,
                1, 0,
                1, 1,
                0, 1,
        };
        
        glVertexPointer(3, GL_FLOAT, 0, vertices);
        glTexCoordPointer(2, GL_FLOAT, 0, texCoords);
        glEnableClientState(GL_VERTEX_ARRAY);
        glEnableClientState(GL_TEXTURE_COORD_ARRAY);

        glDrawArrays(GL_TRIANGLE_FAN, 0, 4);

        glDisableClientState(GL_VERTEX_ARRAY);
        glDisableClientState(GL_TEXTURE_COORD_ARRAY);
        
        glColor4x(1, 1, 1, 0); // reset
    glDisable( GL_TEXTURE_2D );
    glPopMatrix();
    
    
    TRY_RETHROW;
}


