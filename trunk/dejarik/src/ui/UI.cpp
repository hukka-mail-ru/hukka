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
    
    vector<PiecePtr> pieces = mGame->getBoard()->getPieces();
    vector<string> names;
    for(unsigned i=0; i<pieces.size(); i++)
    {
         names.push_back(pieces[i]->name);
    }
    
    // create representation of the cells
    vector<CellPtr> cells;
    mGame->getBoard()->getCells(cells);
    for(unsigned i=0; i<cells.size(); i++)
    {
        createCell(cells[i]);
    }
        
    mVideo.startup(names);
    TRY_RETHROW;
}



void UI::createCell(const CellPtr cell)
{
    // CENTRAL CIRCLE
    if(cell->c == 0)
    {
        for(unsigned i = 0; i < INTERIM_ANGLES*3; i++)
        {
            float a = M_PI/6.0*i;
            cell->x.push_back(CIRCLE_CENTER_X + RADIUS_1 * cos(a));
            cell->y.push_back(CIRCLE_CENTER_Y + RADIUS_1 * sin(a));
        }
        
        cell->x_center = CIRCLE_CENTER_X;
        cell->y_center = CIRCLE_CENTER_Y;
    }
    // CIRCLE 1
    else if(cell->c == 1)
    {
        createCellSegment(cell, RADIUS_1, RADIUS_2);
    }
    // CIRCLE 2
    else if(cell->c == 2)
    {        
        createCellSegment(cell, RADIUS_2, RADIUS_3);
    }
}

// a helper for createCell
void UI::createCellSegment(const CellPtr cell, float radius1, float radius2)
{
    // angles
    const float a1 = M_PI/6.0 * cell->r;
    const float a2 = a1 + M_PI/6.0;
    const float interim = (a2 - a1)/INTERIM_ANGLES;        
    
    float a = a1;
    for(unsigned i = 0; i<=INTERIM_ANGLES; i++) // BBBBBB
    {
        cell->x.push_back(CIRCLE_CENTER_X + radius2 * cos(a));
        cell->y.push_back(CIRCLE_CENTER_Y + radius2 * sin(a));
        a += interim;
    }

    for(unsigned i = 0; i<=INTERIM_ANGLES; i++) // DDDDDD
    {            
        a -= interim;
        cell->x.push_back(CIRCLE_CENTER_X + radius1 * cos(a));
        cell->y.push_back(CIRCLE_CENTER_Y + radius1 * sin(a));
    }
    
    // define center
    cell->x_center = CIRCLE_CENTER_X + (radius2 + radius1)/2 * cos((a2 + a1)/2);
    cell->y_center = CIRCLE_CENTER_Y + (radius2 + radius1)/2 * sin((a2 + a1)/2);
    
    
}


bool UI::isCellClicked(int x, int y, CellPtr& cell)
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


void UI::drawCell(const CellPtr& cell, bool clicked) 
{
    TRY_BEGINS;
    
    RGBA_Color color = RGBA_Color(1,1,1,1);
    

    switch(cell->selected)
    {
        case SEL_CLICKED:         color = RGBA_Color(0,0,1,0.5);  if(!clicked) return; break;
        case SEL_POSSIBLE_MOVE:   color = RGBA_Color(0.2, 1, 0.2,0.5);  if(clicked) return; break;
        case SEL_POSSIBLE_TARGET: color = RGBA_Color(1,0,0,0.5);  if(clicked) return; break;
        case SEL_POSSIBLE_PUSH:   color = RGBA_Color(1,0,1,0.5);  if(clicked) return; break;
      //  case SEL_NONE:            color = RGBA_Color(1,0,1,1);break;
        case SEL_NONE:            return;
    }

    vector<GLshort> vertex;
    for(unsigned i = 0; i<cell->x.size(); i++)
    {
        vertex.push_back(cell->x[i]);
        vertex.push_back(cell->y[i]);
    }
    
    mVideo.drawSolidPolygon(&vertex[0], vertex.size()/2, color);
    
    if(cell->c == 0) 
       mVideo.drawLineLoop(&vertex[0], vertex.size()/2, RGBA_Color(0,0,0,1), 1);
    
       
    TRY_RETHROW;
}


void UI::drawPiece(const PiecePtr& piece)
{
    TRY_BEGINS;
    
    if(mMoving && piece != mMovedPiece) // optimization: draw only the moving piece
        return;
    
    RGBA_Color color = (piece->player.get() == mGame->getActivePlayer()) ?
            RGBA_Color(1,0,1,1) : RGBA_Color(1,1,1,1);

    if(mMoving)
    {
        color = RGBA_Color(1,1,1,1);
    }
    

    ostringstream name;
    name << piece->name << piece->sprite;
    mVideo.drawSprite(name.str(), color, XY_CENTER,
                      piece->x,
                      piece->y,
                      piece->angle); 
        
    TRY_RETHROW;
}


void UI::drawMenu()
{
    TRY_BEGINS;
    
    // TODO some subscriptions in the menu
    mVideo.drawSprite("menu1", RGBA_Color(1,1,1,1), XY_RIGHT_TOP, 0, -BOARD_TEXTURE_WIDTH, 0);
    mVideo.drawSprite("menu2", RGBA_Color(1,1,1,1), XY_LEFT_TOP, 0, -BOARD_TEXTURE_WIDTH, 0);

    TRY_RETHROW;
}

void UI::drawBoard()
{
    TRY_BEGINS;
    mVideo.drawSprite("board1", RGBA_Color(1,1,1,1), XY_RIGHT_TOP, 0, BOARD_TEXTURE_WIDTH, 0);
    mVideo.drawSprite("board2", RGBA_Color(1,1,1,1), XY_LEFT_TOP, 0, BOARD_TEXTURE_WIDTH, 0);
    mVideo.drawSprite("board3", RGBA_Color(1,1,1,1), XY_RIGHT_TOP, 0, 0, 0);
    mVideo.drawSprite("board4", RGBA_Color(1,1,1,1), XY_LEFT_TOP, 0, 0, 0);
     
    TRY_RETHROW;
}


/* Here goes our drawing code */
bool UI::drawAll()
{
    TRY_BEGINS;

    static bool flag = true; // TODO temporary optimized
    if(flag)
    {
        drawMenu();
        flag = false;
    }

    
    static bool init = true;    
    if(init)
    {
        drawBoard();
    }
    else
    {
        mVideo.drawSprite("field1", RGBA_Color(1,1,1,1), XY_RIGHT_BOTTOM, 0, 0, 0);
      //  mVideo.drawSprite("field2", RGBA_Color(1,1,1,1), XY_LEFT_BOTTOM, 0, 0, 0);
     //   mVideo.drawSprite("field3", RGBA_Color(1,1,1,1), XY_RIGHT_TOP, 0, 0, 0);
   //     mVideo.drawSprite("field4", RGBA_Color(1,1,1,1), XY_LEFT_BOTTOM, 0, 0, 0);
    }

    mVideo.enableBlend();
    vector<CellPtr> cells;
    mGame->getBoard()->getCells(cells);
    
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
    
    mVideo.disableBlend();
    
    if(init)
    {
        mVideo.copyBuffer("field1", -7, 320-128);
    //    mVideo.copyBuffer("field2", -7+128, 320-128);
    //    mVideo.copyBuffer("field3", -7, 320-128-128);
     //   mVideo.copyBuffer("field4", -7+128, 320-128-128);
        init = false;
    }
    
    EDR_SwapBuffers();

    TRY_RETHROW;
    
    return true;
}


void UI::onMouseClick(int x, int y)
{
    TRY_BEGINS;
    
    // the coordinates center is the circle center
    x -= WINDOW_WIDTH/2;
    y = BOARD_TEXTURE_WIDTH - y;
     
    CellPtr cell;
    if(isCellClicked(x, y, cell))
    {
        // show Menu item
        menuItemName = (cell->piece) ? cell->piece->name : "default";
        
        // process the click
        BattleResult res = mGame->onCellClick(cell);
       
        if(res == RES_MOVE)
        {
            mGame->getBoard()->getMoveSteps(cell, mMoveSteps);
            mMoveSteps.insert(mMoveSteps.begin(), cell->piece->cellBeforeMoving);
            mMovedPiece = cell->piece;
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
            cout << "GAME OVER. Vinner: " << vinner->getName() << endl;
            exit(0);
        }
        
    }   
 
    
    TRY_RETHROW;
}

void UI::handleEvents()
{
    TRY_BEGINS;
    

    for(;;)
    {
        long time1 = EDR_GetTime(); // start the timer 

        mMoving = animation.updateAll(mMoveSteps);     

        

        static bool afterMove = true;
        if(mMoving)
        {
            long time4 = EDR_GetTime(); // start the timer 
            drawAll();
            long time2 = EDR_GetTime(); // start the timer 

            afterMove = true;
            
        }
        else if(!mMoving && afterMove) // drawAll one more time after moving
        {
            drawAll();
            afterMove = false;
        }


        

        // handle the events in the queue 
        EDR_Event event;
        if(EDR_PollEvent(event))
        {
            if( !mMoving && event.type == EVENT_LBUTTONDOWN) 
            {
                onMouseClick(event.button.x, event.button.y);
                drawAll();
            }
            else if(event.type == EVENT_QUIT || event.type == EVENT_RBUTTONDOWN) 
            {
                break;
            }
        }
       
        // a delay before the next iteration
        
        long drawingTime = EDR_GetTime() - time1;
        if(mMoving && drawingTime < MAX_FRAME_TIME) // we shouldn't draw too fast
        {
        //    EDR_Millisleep(MAX_FRAME_TIME - drawingTime);
        }
        
        if(!mMoving) // we shouldn't call EDR_PollEvent too often.
        {
            EDR_Millisleep(MAX_FRAME_TIME);
        }
        
    }
    
    /* clean ourselves up and exit */
   // mVideo.stop();
    
    TRY_RETHROW;
}





