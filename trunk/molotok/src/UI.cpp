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
    
    mVideo.drawSprite("bg", RGBA_Color(1,1,1,1), XY_LEFT_TOP, -WINDOW_WIDTH/2, WINDOW_HEIGHT/2, 0);
    
    mVideo.enableBlend();
    mVideo.drawSprite("molotok", RGBA_Color(1,1,1,1), XY_CENTER, mX, mY, mAngle);
    mVideo.disableBlend();
    
    SDL_GL_SwapBuffers();

    TRY_RETHROW;
    
    return true;
}


bool mQuit = false;
bool mAnimation = false;


void* catchEvents(void* arg)
{
    while(!mQuit)
    {
        SDL_Event event;
        /* handle the events in the queue */
        while ( SDL_PollEvent( &event ) )
        {
            if( event.type == SDL_MOUSEBUTTONDOWN ) 
            {
                if( event.button.button == SDL_BUTTON_LEFT ) 
                { 
                    mAnimation = true;
                    //onMouseClick(event.button.x, event.button.y)                    
                }

            }
            else if(event.type == SDL_QUIT) // handle stop
            {
                mQuit = true;
            }
        }
    }
    
    return NULL;
}

void UI::handleEvents()
{
    TRY_BEGINS;
    
    /* catch events in a separated thred */
    pthread_t thread;
    pthread_create(&thread, NULL, catchEvents, NULL);
    pthread_detach(thread);
    

    while(!mQuit)
    {

        // animation
        if(mAnimation)
        {
            mAngle += 1;
        }
        if(mAngle > 90)
        {
            mAnimation = false;
        }
        
        
        drawAll();
        
        // a delay before the next iteration
        SDL_Delay(50);
        
    }
    
    /* clean ourselves up and exit */
    mVideo.stop();
    
    TRY_RETHROW;
}





