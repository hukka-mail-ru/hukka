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
    
    for(int j =0; j<4; j++)
    for(int i =0; i<5; i++)
    {
        mVideo.drawSprite("tile", RGBA_Color(1,1,1,1), XY_LEFT_TOP, 
                -WINDOW_WIDTH/2 + 128*i, WINDOW_HEIGHT/2- 128*j, 0);
    }
    
    mVideo.enableBlend();
    mVideo.drawSprite("molotok", RGBA_Color(1,1,1,1), XY_CENTER, mX, mY, mAngle);
    mVideo.disableBlend();
    
    SDL_GL_SwapBuffers();

    TRY_RETHROW;
    
    return true;
}





void UI::handleEvents()
{
    TRY_BEGINS;
    
    bool quit = false;
    bool animation = false;
    bool init = true;

    while(!quit)
    {

        // animation
        if(animation)
        {
            mAngle ++;
        }
        if(mAngle > 90)
        {
            animation = false;
        }
        
        if(init || animation)
        {
            drawAll();
            init = false;
        }
        
        SDL_Event event;
        /* handle the events in the queue */
        while ( SDL_PollEvent( &event ) )
        {
            switch(event.type)
            {
                case SDL_MOUSEBUTTONDOWN: 
                    if( event.button.button == SDL_BUTTON_LEFT ) 
                    { 
                        animation = true;
                    }
                    break;
                case SDL_QUIT:
                    quit = true;
                    break;
                case SDL_VIDEOEXPOSE:
                    drawAll();
                    break;
            }
        }
        
        // a delay before the next iteration
        SDL_Delay(10);
        
    }
    
    /* clean ourselves up and exit */
    mVideo.stop();
    
    TRY_RETHROW;
}





