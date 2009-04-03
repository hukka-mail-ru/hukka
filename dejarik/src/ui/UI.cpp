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
 

#include "UI.h"
#include "System.h"

// TODO Hmm.. VERT_OFFSET should be in Cell.h ?
#define VERT_OFFSET -32.0 // how many pixels is from the board center to the screen center 

using namespace std;


void UI::startup()
{
    TRY_BEGINS;
    
    vector<PiecePtr> pieces = mGame->getBoard()->getPieces();
    vector<string> names;
    for(unsigned i=0; i<pieces.size(); i++)
    {
         names.push_back(pieces[i]->name);
    }
        
    mVideo.startup(names);
    TRY_RETHROW;
}

bool UI::isCellClicked(float x, float y, CellPtr& cell)
{
    TRY_BEGINS;
    
    y = y - VERT_OFFSET;
    
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
            
            for (unsigned i = 0; i < cells[k]->x.size(); i++) 
            {
                cout << "cell - " << k << " : " <<  cells[k]->x[i] << " : " << cells[k]->y[i] << endl;
            }
            
            cout << "cell " << cell->c <<  "." << cell->r << endl; 
            
            return true;
        }
    }
    
    TRY_RETHROW;    
    
    return false;

}

/*
void UI::drawPiece(const PiecePtr& piece)
{
    TRY_BEGINS;
    
    RGB_Color color = (piece->player.get() == mGame->getActivePlayer()) ? RGB_Color(1,0,1) : RGB_Color(1,1,1);

    if(mMoving)
    {
        color = RGB_Color(1,1,1);
    }
    

    ostringstream name;
    name << piece->name << piece->sprite;
    mVideo.drawSprite(name.str(), color, XY_CENTER,
                      piece->x,
                      piece->y + VERT_OFFSET,
                      piece->angle); 
        
    TRY_RETHROW;
}

void UI::drawMenu()
{
    TRY_BEGINS;
   
    
    // menu 
    mVideo.drawSprite("menu_" + menuItemName, RGB_Color(1,1,1), XY_LEFTBOTTOM,
                      4,
                      254,
                      0); 
    
    TRY_RETHROW;
}



void UI::drawCell(const CellPtr& cell, bool clicked) 
{
    TRY_BEGINS;
    
    RGB_Color color = RGB_Color(1,1,1);
    

    switch(cell->selected)
    {
        case SEL_CLICKED:         color = RGB_Color(0,0,1);  if(!clicked) return;  break;
        case SEL_POSSIBLE_MOVE:   color = RGB_Color(0.2, 1, 0.2);  if(clicked) return; break;
        case SEL_POSSIBLE_TARGET: color = RGB_Color(1,0,0);  if(clicked) return; break;
        case SEL_POSSIBLE_PUSH:   color = RGB_Color(1,0,1);  if(clicked) return; break;
        case SEL_NONE:            return;
    }

    vector<float> celly;
    for(unsigned i = 0; i<cell->y.size(); i++)
        celly.push_back(cell->y[i] + VERT_OFFSET);
    
    mVideo.drawPolygon(cell->x, celly, color, 0.5);
    
    if(cell->c == 0) 
       mVideo.drawShape(cell->x, celly, RGB_Color(0,0,0), 1);
    
       
    TRY_RETHROW;
}

*/

void UI::drawBoard()
{
    TRY_BEGINS;
    
    mVideo.drawSprite("board1", RGBA_Color(1,1,1,1), XY_RIGHT_BOTTOM, 0, 0, 0);
    mVideo.drawSprite("board2", RGBA_Color(1,1,1,1), XY_LEFT_BOTTOM, 0, 0, 0);
    mVideo.drawSprite("board3", RGBA_Color(1,1,1,1), XY_RIGHT_TOP, 0, 0, 0);
    mVideo.drawSprite("board4", RGBA_Color(1,1,1,1), XY_LEFT_TOP, 0, 0, 0);
    
    mVideo.drawSprite("Molator0", RGBA_Color(1,1,1,1), XY_LEFT_BOTTOM, 45, 45, 0);
    
    vector<CellPtr> cells;
    mGame->getBoard()->getCells(cells);
    /*
    // draw all but clicked cell    
    for(unsigned i = 0; i < cells.size(); i++)
    {
        drawCell(cells[i], false);
    }
    
    // draw clicked cell
    for(unsigned i = 0; i < cells.size(); i++)
    {
        drawCell(cells[i], true);
    }
    
    // draw Pieces
    vector<PiecePtr> pieces = mGame->getBoard()->getPieces();
    for(unsigned i = 0; i < pieces.size(); i++)
    {
        drawPiece(pieces[i]);
    }
    */
    
     
    TRY_RETHROW;
}

/*
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
}*/


/* Here goes our drawing code */
bool UI::drawAll()
{
    TRY_BEGINS;
    /*
    mVideo.drawImage("board", RGBA_Color(1,1,1,0.5), 0, 0, 45);
    //////// TEST BLOCK ////////////////////

    GLshort vertexArray[] = {
            -10,-10, 0,   
            10, -10,0,     
            10,10,0,
            -10,10,0 };


    mVideo.drawPolygon(&vertexArray[0], 4, RGBA_Color(1,1,1,0.5));
    
    GLshort vertexArray2[] = 
    {
        0,0,0,   
        -40,0,0,     
        -40,-40,0,
        0,-40,0 
    };

    mVideo.drawPolygon(&vertexArray2[0], 4, RGBA_Color(1,0,1,0.5));
    */
    
    drawBoard();
   
    //////////////////////////////////
    
    /*
    
    mVideo.drawBackground();
    drawBoard();
    drawMenu();
   */
    
    /* Draw it to the screen */
    if(!mQuit)
    {       
        swapBuffers();
    }

    TRY_RETHROW;
    
    return true;
}


void UI::onMouseClick(int button_x, int button_y)
{
    TRY_BEGINS;
     
    CellPtr cell;
    if(isCellClicked(button_x, button_y, cell))
    {
       // cout << "mouse " << event.button.x << "." << event.button.y << endl;
        //cout << "cell " << cell->c << "." << cell->r << endl;
        //if(cell->piece)
        //    cout << "piece " << cell->piece->name << " move " <<cell->piece->moveRating << endl; 
        
        // show Menu item
        menuItemName = (cell->piece) ? cell->piece->name : "default";
        
        // process the click
        BattleResult res = mGame->onCellClick(cell);
       
        if(res == RES_MOVE)
        {
            mGame->getBoard()->getMoveSteps(cell, mMoveSteps);
            mMoveSteps.insert(mMoveSteps.begin(), cell->piece->cellBeforeMoving);
            
        }
        
        switch(res)
        {
            case RES_CLICK: cout << "RES_CLICK" << endl; break;
            case RES_MOVE: cout << "RES_MOVE" << endl; break;
            case RES_KILL: menuItemName = "kill"; cout << "RES_KILL" << endl;break;
            case RES_PUSH: menuItemName = "push"; cout << "RES_PUSH" << endl;break;
            case RES_COUNTER_KILL: menuItemName = "counter_kill"; cout << "RES_COUNTER_KILL" << endl;break;
            case RES_COUNTER_PUSH: menuItemName = "counter_push"; cout << "RES_COUNTER_PUSH" << endl;break;
            default: break;
        }
        
        PlayerPtr vinner;
        if(mGame->checkVictory(vinner))
        {
            mQuit = true;
            cout << "GAME OVER. Vinner: " << vinner->getName() << endl;
        }
        
    }   
 
    
    TRY_RETHROW;
}


void UI::handleEvents()
{
    TRY_BEGINS;
    
    unsigned ticks = 0;
    while ( !mQuit )
    {
        long time1 = getTime(); // start the timer 

        mMoving = animation.updateAll(mMoveSteps);
        
        drawAll();
             
        sleep(1);
        
        if(ticks++ > 5)
        	mQuit = true;
        
        /* handle the events in the queue */
        /*
        SDL_Event event;
        while ( SDL_PollEvent( &event ) )
        {
            if( !mMoving && event.type == SDL_MOUSEBUTTONDOWN &&
                 event.button.button == SDL_BUTTON_LEFT ) 
            {
                onMouseClick(event.button.x, event.button.y);
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
       
        // a delay before the next iteration
        if(mMoving)
        {
            float timeForDrawing = (getTime() - time1)/1000.0;
            SDL_Delay(20 - timeForDrawing);
        }
        else
        {
            SDL_Delay(1); // to prevent too frequent drawings
        }
         */
        
    }
    
    /* clean ourselves up and exit */
   // mVideo.stop();
    
    TRY_RETHROW;
}





