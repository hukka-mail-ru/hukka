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

#include "SDL.h"
#include "UI.h"


/* screen width, height, and bit depth */
#define SCREEN_WIDTH  240
#define SCREEN_HEIGHT 320
#define SCREEN_BPP     16


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


void UI::mouseToGL(float winX, float winY, GLdouble& x, GLdouble& y, GLdouble& z)
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



bool UI::isCellClicked(GLdouble x, GLdouble y, unsigned& c, unsigned& r)
{
    TRY_BEGINS;
    
    vector<CellPtr> cells;
    mGame->getBoard()->getCells(cells);
    
    for(unsigned i = 0; i < cells.size(); i++)
    {
        unsigned npol = cells[i]->x.size();
        vector<float>& xp = cells[i]->x;
        vector<float>& yp = cells[i]->y;

        // is (x,y) inside the polygon (xp, yp)
        bool res = false;
        for (int i = 0, j = npol - 1; i < npol; j = i++) 
        {
          if ((((yp[i]<=y) && (y<yp[j])) || ((yp[j]<=y) && (y<yp[i]))) &&
            (x > (xp[j] - xp[i]) * (y - yp[i]) / (yp[j] - yp[i]) + xp[i]))
              res = !res;
        }
        
        if(res)
        {
            c = cells[i]->c;
            r = cells[i]->r;
            return true;
        }
    }
    
    TRY_RETHROW;    
    
    return false;

}


void UI::drawCell(const CellPtr& cell) 
{
    TRY_BEGINS;
       

    // cells must be back/white like a chess
    unsigned rest = (cell->c == 0 || cell->c == 1) ? 0 : 1;     
    if(cell->r % 2 == rest) // odd/even
    {
        glColor3f(1.0f,1.0f,1.0f);
    }
    else
    {
        glColor3f(0.0f,0.0f,0.0f);
    }
    
    glBegin( GL_POLYGON );    
        for(unsigned i = 0; i < cell->x.size(); i++)
        {
            glVertex3f( cell->x[i],  cell->y[i], 0.0f );
        }
    glEnd( ); 
    
    TRY_RETHROW;
}



void UI::drawBoard()
{
    TRY_BEGINS;
    
    vector<CellPtr> cells;
    mGame->getBoard()->getCells(cells);
    
    for(unsigned i = 0; i < cells.size(); i++)
    {
        drawCell(cells[i]);
    }
    
    TRY_RETHROW;
}




/* Here goes our drawing code */
bool UI::drawAll()
{
    TRY_BEGINS;
    
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

    TRY_RETHROW;
    
    return true;
}



void UI::handleEvents()
{
    TRY_BEGINS;
    
    /* wait for events */
    SDL_Event event;

    while ( !mQuit )
    {
        drawAll();

        /* handle the events in the queue */
        while ( SDL_PollEvent( &event ) )
        {
            // KEY EVENT
            if( event.type == SDL_KEYDOWN ) // handle key pressed 
            {
                if( event.key.keysym.sym  == SDLK_ESCAPE)
                {
                    mQuit = true;
                    break;
                }
            }
            
            // MOUSE EVENT
            else if( event.type == SDL_MOUSEBUTTONDOWN ) 
            {
                if( event.button.button == SDL_BUTTON_LEFT ) 
                { 
                    GLdouble x = 0;
                    GLdouble y = 0;
                    GLdouble z = 0;
                    mouseToGL(event.button.x, event.button.y, x, y, z);
                    
                    unsigned cellC = 0;
                    unsigned cellX = 0;
                    if(isCellClicked(x, y, cellC, cellX))
                    {
                        cout << "cell " << cellC << "." << cellX << endl;
                        mGame->onCellClick(cellC, cellX);
                    }   
                }
            }

            else if(event.type == SDL_QUIT) // handle stop
            {
                mQuit = true;
            }
        }
    
        
    }
    
    /* clean ourselves up and exit */
    stop(true);
    
    TRY_RETHROW;
}





