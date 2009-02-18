/*
 * This code was created by Jeff Molofee '99 
 * (ported to Linux/SDL by Ti Leggett '01)
 *
 * If you've found this code useful, please let me know.
 *
 * Visit Jeff at http://nehe.gamedev.net/
 * 
 * or for port-specific comments, questions, bugreports etc. 
 * email to leggett@eecs.tulane.edu
 */
 
#include <iostream>
#include <stdlib.h>
#include <math.h>
#include <GL/gl.h>
#include <GL/glu.h>

#include "SDL.h"
#include "UI.h"


/* screen width, height, and bit depth */
#define SCREEN_WIDTH  240
#define SCREEN_HEIGHT 320
#define SCREEN_BPP     16

#define PI 3.14159265

using namespace std;


// TODO replace cerr with THROW

bool UI::startup()
{
    /* initialize SDL */
    if ( SDL_Init( SDL_INIT_VIDEO ) < 0 )
    {
        cerr << "Video initialization failed: " <<  SDL_GetError( ) << endl;
        return stop(false);
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



void UI::waitForEvents()
{
    /* wait for events */
    SDL_Event event;

    while ( !mQuit )
    {
        drawAll();

        /* handle the events in the queue */
        while ( SDL_PollEvent( &event ) )
        {
            if( event.type == SDL_KEYDOWN ) // handle key pressed 
            {
                if( event.key.keysym.sym  == SDLK_ESCAPE)
                {
                    mQuit = true;
                    break;
                }
            }
            if( event.type == SDL_MOUSEBUTTONDOWN ) 
            {
                GLfloat winX  = 0;
                GLfloat winY = 0;
                GLfloat winZ = 0;
                if( event.button.button == SDL_BUTTON_LEFT ) 
                { 
                    //Get the mouse offsets 
                    winX = event.button.x; 
                    winY = event.button.y; 
                    
                    cout << "winX: " << winX << endl; 
                    cout << "winY: " << winY << endl; 

                }
                
                GLint viewport[4];                  // Where The Viewport Values Will Be Stored
                glGetIntegerv(GL_VIEWPORT, viewport); // Retrieves The Viewport Values (X, Y, Width, Height)
                
                GLdouble modelview[16];                 // Where The 16 Doubles Of The Modelview Matrix Are To Be Stored
                glGetDoublev(GL_MODELVIEW_MATRIX, modelview);       // Retrieve The Modelview Matrix

                GLdouble projection[16];                // Where The 16 Doubles Of The Projection Matrix Are To Be Stored
                glGetDoublev(GL_PROJECTION_MATRIX, projection);     // Retrieve The Projection Matrix

                winY = (float)viewport[3] - winY;           // Subtract The Current Mouse Y Coordinate From The Screen Height
                
                glReadPixels(winX, winY, 1, 1, GL_DEPTH_COMPONENT, GL_FLOAT, &winZ);
                
                GLdouble posX, posY, posZ;              // Hold The Final Values
                gluUnProject( winX, winY, winZ, modelview, projection, viewport, 
                              &posX, &posY, &posZ);

                cout << "posX: " << posX << endl; 
                cout << "posY: " << posY << endl; 
                cout << "posZ: " << posZ << endl; 
            }
            /*if( event.type == SDL_MOUSEDOWN ) // mouse button pressed
            {
                unsigned cellC = 0; // cell coordinate
                unsigned cellX = 0; // cell coordinate
                if(mGame.isCell(x, y, cellC, cellX))
                {
                    mGame.onCellClick(cellC, cellX);
                }   
            }*/
            else if(event.type == SDL_QUIT) // handle stop
            {
                mQuit = true;
            }
        }
    
        
    }
    
    /* clean ourselves up and exit */
    stop(true);
}



/* function to release/destroy our resources and restoring the old desktop */
bool UI::stop( bool res )
{
    SDL_Quit( );
    return res;
}

/* function to reset our viewport after a window resize */
bool UI::resizeWindow(unsigned width, unsigned height )
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

/* general OpenGL initialization function */
bool UI::initGL()
{

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



void UI::drawCell(Color color, float x1, float x2, float a1) // a = init corner
{
    float a2 = a1 + PI/6.0;
    
    if(color == CL_BLACK)
    {
        glColor3f(0.0f,0.0f,0.0f);
    }
    else if(color == CL_WHITE)
    {
        glColor3f(1.0f,1.0f,1.0f);
    }
    
    glBegin( GL_QUADS );      
        glVertex3f( x1 * cos(a1),  x1 * sin(a1), 0.0f );
        glVertex3f( x2 * cos(a1),  x2 * sin(a1), 0.0f );
        glVertex3f( x2 * cos(a2),  x2 * sin(a2), 0.0f );
        glVertex3f( x1 * cos(a2),  x1 * sin(a2), 0.0f );
    glEnd( );  
}


void UI::drawBoard()
{
    // the cells
    float x = 0.5;
    for(unsigned i = 1; i < 12; i+= 2)
    {
        drawCell(CL_WHITE, x*1, x*3, PI/6.0*(i));
        drawCell(CL_WHITE, x*3, x*5, PI/6.0*(i + 1));
        drawCell(CL_BLACK, x*1, x*3, PI/6.0*(i + 1));
        drawCell(CL_BLACK, x*3, x*5, PI/6.0*(i + 2));
    }  
    
    // the central cell 
    glColor3f(0.0f,0.0f,0.0f);
    glBegin( GL_POLYGON ); 
        for(unsigned i = 0; i < 12; i++)
        {
            glVertex3f( x * cos(PI/6.0*i),  x * sin(PI/6.0*i), 0.0f );
        }
    glEnd( );  
    
    glColor3f(1.0f,1.0f,1.0f);
    glBegin( GL_POLYGON ); 
        for(unsigned i = 0; i < 12; i++)
        {
            glVertex3f( (x-0.03) * cos(PI/6.0*i),  (x-0.03) * sin(PI/6.0*i), 0.0f );
        }
    glEnd( );  
}




/* Here goes our drawing code */
bool UI::drawAll()
{
    /* Clear The Screen And The Depth Buffer */
    glClear( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
    glLoadIdentity();
    glTranslatef( 0.0f, 0.0f, -10.0f );

    drawBoard();

    
    /* Draw it to the screen */
    if(!mQuit)
    {
        SDL_GL_SwapBuffers();
    }

    return true;
}




