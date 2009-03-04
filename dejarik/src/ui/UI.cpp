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
 

#include <stdlib.h>
#include <math.h>
#include "UI.h"
#include "Video.h"


using namespace std;


// TODO replace cerr with THROW

void UI::startup()
{
    TRY_BEGINS;
    
    vector<PiecePtr> pieces = mGame->getPieces();
    vector<string> names;
    for(unsigned i=0; i<pieces.size(); i++)
    {
         names.push_back(pieces[i]->name);
    }
        
    Video::startup(names);
    TRY_RETHROW;
}

bool UI::isCellClicked(float x, float y, CellPtr& cell)
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

// helper for drawPiece
float UI::getNormalAngle(float x, float y)
{
    const float dy = y - CIRCLE_CENTER_Y;
    const float dx = x - CIRCLE_CENTER_X;
    
    
    
    float ang = atan (- dy / dx ) * 180.0 / PI;
    if(ang > 0)
        ang += 180;
    
    if(dy < 0)
        ang += 180;
    
    ang +=90;
    
    return ang;
}

// helper for drawPiece
float UI::getRotation(unsigned step)
{
    float rotation = 0.0;
    if(mMoveSteps[step]->c > mMoveSteps[step+1]->c) // look to the center
    {
        rotation = 0;
    }
    else if(mMoveSteps[step]->c < mMoveSteps[step+1]->c) // look to the outer space
    {
        rotation = 180.0;
    }
    else if(mMoveSteps[step]->r == RADIUSES-1 && mMoveSteps[step+1]->r == 0) // boundary
    {
        rotation = 90.0;
    }
    else if(mMoveSteps[step]->r == 0 && mMoveSteps[step+1]->r == RADIUSES-1) // boundary
    {
        rotation = -90.0;
    }
    else if(mMoveSteps[step]->r < mMoveSteps[step+1]->r)// look left
    {
        rotation = 90.0;
    }
    else if(mMoveSteps[step]->r > mMoveSteps[step+1]->r)// look right
    {
        rotation = - 90.0;
    }
    
    return rotation;
}

float shorterAngle(float ang)
{

    if(ang > 180)
        return 180 - ang;
    
    if(ang < -180)
        return 360 + ang;

    return ang;
}

void UI::drawPiece(const CellPtr& cell)
{
    TRY_BEGINS;
    
    if(!cell->piece)
        return;

    RGB color = (cell->piece->player.get() == mGame->getPlayer1()) ? RGB(1,1,1) : RGB(1,0,0);

    const unsigned rot = 20;
    const unsigned straight = 20;
    const unsigned total = rot + straight;
   
    static unsigned moves = 0; // pixel by pixel
    static unsigned step = 0; // cell by cell

    if(cell->piece->cellBeforeMoving != cell) // moving needed
    {
        assert(mMoveSteps.size() > 1);    
        
        
        //  without change of direction
        if(moves < rot && (int)cell->piece->angle == 
            (int)(getNormalAngle(cell->piece->x, cell->piece->y) + getRotation(step)) )
        {
            moves = rot;
        }
        
        // rotation at the beginning
        if(moves < rot)
        {            
            float a_start = cell->piece->angle;            
            float a_finish = getNormalAngle(cell->piece->x, cell->piece->y) + getRotation(step);
            
            if(mMoveSteps[step]->c == 0) // special case - when we start from the center
            {
                a_finish = getNormalAngle(mMoveSteps[step+1]->x_center, mMoveSteps[step+1]->y_center); 
                           + getRotation(step);
                   
                a_finish = shorterAngle(a_finish);
                
                cout << "a_start: " << a_start << endl;
                cout << "a_finish: " << a_finish << endl;
            }          
            
            const float a = a_start + (a_finish - a_start) / rot * moves;
            
            cell->piece->angle = a;
        }
        
        // then move straight
        if(moves >= rot && moves < total)
        {
            const float x_start = mMoveSteps[step]->x_center; 
            const float y_start = mMoveSteps[step]->y_center; 
            
            const float x_finish = mMoveSteps[step+1]->x_center; 
            const float y_finish = mMoveSteps[step+1]->y_center; 
            
            float x = x_start + (x_finish - x_start) / straight * (moves - rot);
            float y = y_start + (y_finish - y_start) / straight * (moves - rot);
            
            // smooth rotation     
            cell->piece->angle = getNormalAngle(x, y) + getRotation(step);
            cell->piece->x = x;
            cell->piece->y = y;
        }
        
        
        // draw
        Video::drawSprite(cell->piece->name, color, XY_CENTER, cell->piece->x,  cell->piece->y, cell->piece->angle); 

        moves++;
        
        if(moves >= total) // proceed to the next cell
        {            
            step++;
            moves = 0;
            
            if(step == mMoveSteps.size() - 1) // finish cell reached
            {
                step = 0;
                mMoving = false;
                cell->piece->cellBeforeMoving = cell;
            }
        }
    }    
    else // just draw a piece
    {
        if(cell->piece->angle == FLOAT_UNDEFINED) 
        {
            cell->piece->angle = getNormalAngle(cell->x_center, cell->y_center);
        }
        if(cell->piece->x == FLOAT_UNDEFINED) 
        {
            cell->piece->x = cell->x_center;
        }
        if(cell->piece->y == FLOAT_UNDEFINED) 
        {
            cell->piece->y = cell->y_center;
        }
        
        Video::drawSprite(cell->piece->name, color, XY_CENTER,
                          cell->piece->x,
                          cell->piece->y,
                          cell->piece->angle); // a piece must look at the center*/  
    }
    TRY_RETHROW;
}



void UI::drawCell(const CellPtr& cell, bool clicked) 
{
    TRY_BEGINS;
    
    RGB color = RGB(1,1,1);
    

    switch(cell->selected)
    {
        case SEL_CLICKED:         color = RGB(0,0,1);  if(!clicked) return; break;
        case SEL_POSSIBLE_MOVE:   color = RGB(0.2, 1, 0.2);  if(clicked) return; break;
        case SEL_POSSIBLE_TARGET: color = RGB(1,0,0);  if(clicked) return; break;
        case SEL_POSSIBLE_PUSH:   color = RGB(1,0,1);  if(clicked) return; break;
        case SEL_NONE:            return;
    }

    
    Video::drawPolygon(cell->x, cell->y, color, 0.5);
    
    if(cell->c == 0) 
     Video::drawShape(cell->x, cell->y, RGB(0,0,0), 1);
    
       
    TRY_RETHROW;
}



void UI::drawBoard()
{
    TRY_BEGINS;
    
    Video::drawSprite("board", RGB(1,1,1), XY_LEFTBOTTOM, 1, 1, 0);
    
    
    vector<CellPtr> cells;
    mGame->getBoard()->getCells(cells);
    
    // draw all but clicked cell
    for(unsigned i = 0; i < cells.size(); i++)
    {
        drawCell(cells[i], false);
        drawPiece(cells[i]);
    }
    
    // draw clicked cell
    for(unsigned i = 0; i < cells.size(); i++)
    {
        drawCell(cells[i], true);
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
    
    Video::drawBackground();
    drawBoard();
  //  drawActivePlayer();

    
    
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
    TRY_BEGINS;
    
    if( event.button.button == SDL_BUTTON_LEFT ) 
    {        
        CellPtr cell;
        if(isCellClicked(event.button.x, event.button.y, cell))
        {
            cout << "mouse " << event.button.x << "." << event.button.y << endl;
            //cout << "cell " << cell->c << "." << cell->r << endl;
            //if(cell->piece)
            //    cout << "piece " << cell->piece->name << " move " <<cell->piece->moveRating << endl; 
            
            BattleResult res = mGame->onCellClick(cell);
           
            if(res == RES_MOVE)
            {
                mMoving = true;
                mGame->getBoard()->getMoveSteps(cell, mMoveSteps);
                mMoveSteps.insert(mMoveSteps.begin(), cell->piece->cellBeforeMoving);
                
            }
            
            // log -------------------------
            switch(res)
            {
                case RES_CLICK: cout << "RES_CLICK" << endl; break;
                case RES_MOVE: cout << "RES_MOVE" << endl; break;
                case RES_KILL:  cout << "RES_KILL" << endl;break;
                case RES_PUSH:cout << "RES_PUSH" << endl;break;
                case RES_COUNTER_KILL:cout << "RES_COUNTER_KILL" << endl;break;
                case RES_COUNTER_PUSH:cout << "RES_COUNTER_PUSH" << endl;break;
                default: break;
            }
            //----------------------------
            
            PlayerPtr vinner;
            if(mGame->checkVictory(vinner))
            {
                mQuit = true;
                cout << "GAME OVER. Vinner: " << vinner->getName() << endl;
            }

        }   
    }   
    
    TRY_RETHROW;
}


void UI::handleEvents()
{
    TRY_BEGINS;
    
    SDL_Event event;
    while ( !mQuit )
    {
        drawAll();
        SDL_Delay(1); // to prevent too frequent drawings
        
        if(mMoving)
        {
            SDL_Delay(5);
        }
        
        /* handle the events in the queue */
        while ( SDL_PollEvent( &event ) )
        {
            
            
            // MOUSE EVENT
            if( !mMoving && event.type == SDL_MOUSEBUTTONDOWN ) 
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
    Video::stop();
    
    TRY_RETHROW;
}





