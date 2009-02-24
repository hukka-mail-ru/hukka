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



bool UI::isCellClicked(GLdouble x, GLdouble y, CellPtr& cell)
{
    TRY_BEGINS;
    
    vector<CellPtr> cells;
    mGame->getBoard()->getCells(cells);
    
    for(unsigned k = 0; k < cells.size(); k++)
    {
        unsigned npol = cells[k]->x.size();
        vector<float>& xp = cells[k]->x;
        vector<float>& yp = cells[k]->y;

        // is (x,y) inside the polygon (xp, yp)
        bool res = false;
        for (unsigned i = 0, j = npol - 1; i < npol; j = i++) 
        {
          if ((((yp[i]<=y) && (y<yp[j])) || ((yp[j]<=y) && (y<yp[i]))) &&
            (x > (xp[j] - xp[i]) * (y - yp[i]) / (yp[j] - yp[i]) + xp[i]))
              res = !res;
        }
        
        if(res)
        {
            cell = cells[k];
            return true;
        }
    }
    
    TRY_RETHROW;    
    
    return false;

}


void UI::drawPiece(const CellPtr& cell)
{
    if(!cell->piece)
        return;
    
    if(cell->piece->player.get() == mGame->getPlayer1())
        glColor3f(0.5f ,0.5f, 1.0f); // blue
    else
        glColor3f(1.0f ,0.0f, 1.0f); // pink
    
    glBegin( GL_POLYGON ); 
         float x = cell->x_center;
         float y = cell->y_center;
         float w = 0.1;
         glVertex3f(x+w, y, 0);
         glVertex3f(x, y+w, 0);
         glVertex3f(x-w, y, 0);
         glVertex3f(x, y-w, 0);
    glEnd();
}



void UI::drawCell(const CellPtr& cell) 
{
    TRY_BEGINS;
    
    Color color = CL_WHITE;
    
    switch(cell->selected)
    {
        case SEL_CLICKED:         color = CL_BLUE;  break;
        case SEL_POSSIBLE_MOVE:   color = CL_GREEN; break;
        case SEL_POSSIBLE_TARGET: color = CL_RED;   break;
        case SEL_POSSIBLE_PUSH:   color = CL_RED;   break;
        
        case SEL_NONE:
          {
              // cells must be back/white like a chess
              unsigned rest = (cell->c == 0 || cell->c == 1) ? 0 : 1; // depends on circle    
              color = (cell->r % 2 == rest) ? CL_WHITE : CL_BLACK; // odd/even
              break;
          }
    }
    
    switch(color)
    {
        case CL_WHITE: glColor3f(1.0f,1.0f,1.0f); break;
        case CL_BLACK: glColor3f(0.0f,0.0f,0.0f); break;
        case CL_GREEN: glColor3f(0.0f,1.0f,0.0f); break;
        case CL_BLUE: glColor3f(0.0f,0.0f,1.0f); break;
        case CL_RED: glColor3f(1.0f,0.0f,0.0f); break;
        default: return;
    }
       
    glBegin( GL_POLYGON );    
        for(unsigned i = 0; i < cell->x.size(); i++)
        {
            glVertex3f( cell->x[i],  cell->y[i], 0.0f );
        }
    glEnd(); 
    
    drawPiece(cell);
    
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


void UI::drawActivePlayer()
{
    TRY_BEGINS;
    
    if(!mGame->getActivePlayer())
        return;

    float y = 0;
    if(mGame->getActivePlayer() == mGame->getPlayer1())
    {
        glColor3f(0.5f ,0.5f, 1.0f); // blue
        y = 3;
    }
    else
    {
        glColor3f(1.0f ,0.0f, 1.0f); // pink
        y = -3;
    }
    
    glBegin( GL_POLYGON ); 
         glVertex3f(0, y, 0);
         glVertex3f(1, y, 0);
         glVertex3f(1, y+0.2, 0);
         glVertex3f(0, y+0.2, 0);
    glEnd();
    
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
    drawActivePlayer();
    
    /* Draw it to the screen */
    if(!mQuit)
    {
        SDL_GL_SwapBuffers();
    }

    TRY_RETHROW;
    
    return true;
}


void UI::onMouseClick(const SDL_Event& event)
{
    if( event.button.button == SDL_BUTTON_LEFT ) 
    { 
        GLdouble x = 0;
        GLdouble y = 0;
        GLdouble z = 0;
        mouseToGL(event.button.x, event.button.y, x, y, z);
        
        CellPtr cell;
        if(isCellClicked(x, y, cell))
        {
            cout << "cell " << cell->c << "." << cell->r << endl;
            if(cell->piece)
                cout << "piece " << cell->piece->name << " move " <<cell->piece->moveRating << endl; 
            
            BattleResult res = mGame->onCellClick(cell);
            
            switch(res)
            {
                case RES_NO_BATTLE: cout << "RES_NO_BATTLE" << endl; break;
                case RES_KILL:  cout << "RES_KILL" << endl;break;
                case RES_PUSH:cout << "RES_PUSH" << endl;break;
                case RES_COUNTER_KILL:cout << "RES_COUNTER_KILL" << endl;break;
                case RES_COUNTER_PUSH:cout << "RES_COUNTER_PUSH" << endl;break;
                default: break;
            }
            
            PlayerPtr vinner;
            if(mGame->checkVictory(vinner))
            {
                mQuit = true;
                cout << "GAME OVER. Vinner: " << vinner->getName() << endl;
            }
        }   
    }   
}


void UI::handleEvents()
{
    TRY_BEGINS;
    
    SDL_Event event;
    while ( !mQuit )
    {
        drawAll();

        /* handle the events in the queue */
        while ( SDL_PollEvent( &event ) )
        {
            // MOUSE EVENT
            if( event.type == SDL_MOUSEBUTTONDOWN ) 
            {
                onMouseClick(event);
            }

            else if(event.type == SDL_QUIT) // handle stop
            {
                mQuit = true;
            }
            
            if(mQuit)
            {
                break;
            }
        }
    }
    
    /* clean ourselves up and exit */
    stop(true);
    
    TRY_RETHROW;
}





