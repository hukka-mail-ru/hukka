#include <math.h>

#include "UI.h"
#include "System.h"

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
    /*
    GLshort vertexArray[] = 
    {
            0,0,0,
            0,10,0,
            10,10,0,
            10,0,0
    };
    */
    
    mVideo.drawSprite("molotok", RGBA_Color(1,1,1,1), XY_CENTER, mX, mY, mAngle);
    
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
    bool rotation = false;

    while(!quit)
    {

        // animation
        if(rotation)
        {
            mAngle += 0.5;
        }
        if(mAngle > 90)
        {
            rotation = false;
        }
        
        
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
                    rotation = true;
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
        SDL_Delay(1);
        
    }
    
    /* clean ourselves up and exit */
    mVideo.stop();
    
    TRY_RETHROW;
}





