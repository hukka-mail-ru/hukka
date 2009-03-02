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



void UI::drawPiece(const CellPtr& cell)
{
    TRY_BEGINS;
    
    if(!cell->piece)
        return;
    
    float x_offset = 15; // a half of texture size
    float y_offset = 15; // a half of texture size
    

    RGB color = (cell->piece->player.get() == mGame->getPlayer1()) ? RGB(1,1,1) : RGB(1,0,0);

    Video::drawSprite(cell->piece->name, color, 
                      cell->x_center - x_offset, 
                      cell->y_center - y_offset, (3 - cell->r) * 30 - 15);

    
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
    
    Video::drawSprite("board", RGB(1,1,1), 1, 1, 0);
    
    
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
    
    
  //  drawSquare();
    
    
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
    
    TRY_RETHROW;
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
    Video::stop();
    
    TRY_RETHROW;
}





