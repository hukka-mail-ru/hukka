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

bool UI::startup()
{
    TRY_BEGINS;
    return Video::startup();
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
            return true;
        }
    }
    
    TRY_RETHROW;    
    
    return false;

}


void UI::drawSquare()// temp
{
    TRY_BEGINS;    
    for(int i =0 ;i<360; i+=30)
        Video::drawMaskedSprite(Video::segment1, RGB(1,1,1), 144, 120, i);
    
    for(int i =0 ;i<360; i+=30)
        Video::drawMaskedSprite(Video::segment2, RGB(1,1,1), 186, 102, i);
    
    Video::drawMaskedSprite(Video::segment0, RGB(1,1,1), 92, 132, 0);

    TRY_RETHROW;
}


void UI::drawPiece(const CellPtr& cell)
{
    TRY_BEGINS;
    
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
    
    TRY_RETHROW;
}



void UI::drawCell(const CellPtr& cell) 
{
    TRY_BEGINS;
    
    Color color = CL_WHITE;
    Color backgr = CL_WHITE;
    
    // cells must be back/white like a chess
    unsigned rest = (cell->c == 0 || cell->c == 1) ? 0 : 1; // depends on circle    
    backgr = (cell->r % 2 == rest) ? CL_WHITE : CL_BLACK; // odd/even
    
    
    switch(cell->selected)
    {
        case SEL_CLICKED:         color = CL_BLUE;  break;
        case SEL_POSSIBLE_MOVE:   color = CL_GREEN; break;
        case SEL_POSSIBLE_TARGET: color = CL_RED;   break;
        case SEL_POSSIBLE_PUSH:   color = CL_RED;   break;
        case SEL_NONE:            color = backgr;   break;
    }
    
    switch(color)
    {
        case CL_WHITE: glColor3f(1.0f,1.0f,1.0f); break;
        case CL_BLACK: glColor3f(0.0f,0.0f,0.0f); break;
        case CL_GREEN: if(backgr == CL_WHITE) glColor3f(0.0f,0.8f,0.0f); else glColor3f(0.0f,0.4f,0.0f); break;
        case CL_BLUE: glColor3f(0,0,1); break;
        case CL_RED:  glColor3f(1,0,0); break;
        default: return;
    }
       
    glBegin( GL_POLYGON );    
        for(unsigned i = 0; i < cell->x.size(); i++)
        {
            glVertex3f( cell->x[i],  cell->y[i], 0.0f );
        }
    glEnd(); 
    
       
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
        drawPiece(cells[i]);
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

  //  drawBoard();
  //  drawActivePlayer();
    
    Video::drawBackground();
    drawSquare();
    
    
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
        GLdouble x = 0;
        GLdouble y = 0;
        GLdouble z = 0;
        Video::winToGL(event.button.x, event.button.y, x, y, z);
        
        cout << "mouse " << event.button.x << " " << event.button.y << endl;
        cout << "mouse " << x << " "<< y << " " << z << endl;
        
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
    Video::stop(true);
    
    TRY_RETHROW;
}





