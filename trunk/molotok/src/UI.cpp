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
#include <math.h>

#include "UI.h"
#include "System.h"

#define INTERIM_ANGLES 4 // smoothness of the circles
#define MAX_FRAME_TIME 50 // in milliseconds

using namespace std;


void UI::startup()
{
    TRY_BEGINS;
    
        
    mVideo.startup();
    TRY_RETHROW;
}






/* Here goes our drawing code */
bool UI::drawAll()
{
    TRY_BEGINS;

    // Square coordinates
    GLshort vertexArray[] = 
    {
            0,0,0,
            0,50,0,
            100,50,0,
            100,0,0
    };
    
    
    // draw a scene
    mVideo.drawSolidPolygon(vertexArray, 4, RGBA_Color(1,1,1,1));
    
    SDL_GL_SwapBuffers();

    TRY_RETHROW;
    
    return true;
}


void UI::onMouseClick(int x, int y)
{
    TRY_BEGINS;
    

    
    
    TRY_RETHROW;
}

void UI::handleEvents()
{
    TRY_BEGINS;
    
    /* wait for events */
    SDL_Event event;
    bool quit = false;

    while(!quit)
    {


        drawAll();

        
        /* handle the events in the queue */
        while ( SDL_PollEvent( &event ) )
        {

            if( event.type == SDL_MOUSEBUTTONDOWN ) 
            {
             //   GLfloat winX  = 0;
             //   GLfloat winY = 0;
             //   GLfloat winZ = 0;
                if( event.button.button == SDL_BUTTON_LEFT ) 
                { 
                    //Get the mouse offsets 
               //     winX = event.button.x; 
               //     winY = event.button.y; 
                    
               //     cout << "winX: " << winX << endl; 
               //     cout << "winY: " << winY << endl; 

                }

            }
            else if(event.type == SDL_QUIT) // handle stop
            {
                quit = true;
            }
        }
        
        // a delay before the next iteration
        SDL_Delay(MAX_FRAME_TIME);
        
    }
    
    /* clean ourselves up and exit */
    mVideo.stop();
    
    TRY_RETHROW;
}





