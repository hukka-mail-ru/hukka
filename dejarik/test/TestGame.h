#ifndef TestGameH_
#define TestGameH_


#include <cppunit/extensions/HelperMacros.h>

#include "Player.h"
#include "Macros.h"
#include "Game.h"

class TestGame: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestGame);
   CPPUNIT_TEST(testStart);
   CPPUNIT_TEST(testCheckVictory);
   CPPUNIT_TEST(testOver);
   CPPUNIT_TEST(testSelection);
   CPPUNIT_TEST(testMove);
   CPPUNIT_TEST(testMove2);
   CPPUNIT_TEST(testMove3);
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
    
    
    void testMove3()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Game game;
        game.startup();
        
        BoardPtr board = game.getBoard();
        board->getCell(2, 2)->piece->moveRating = 3;
        
        game.onCellClick(board->getCell(2,2));
        game.onCellClick(board->getCell(0,0));

        CPPUNIT_ASSERT(board->getCell(0,0)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(1,2)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(2,2)->selected == SEL_NONE);
        
        CPPUNIT_ASSERT(!board->getCell(2, 2)->piece);
        CPPUNIT_ASSERT(board->getCell(0, 0)->piece);
        
        TRY_CATCH;
    }
    
    
    void testMove()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Game game;
        game.startup();
        
        BoardPtr board = game.getBoard();
        board->getCell(2, 0)->piece->moveRating = 1;   
        
        // Move 1
        game.onCellClick(board->getCell(2,0));
        CPPUNIT_ASSERT(board->getCell(2,0)->selected == SEL_CLICKED);
        CPPUNIT_ASSERT(board->getCell(1,0)->selected == SEL_POSSIBLE_MOVE);
        CPPUNIT_ASSERT(board->getCell(2,11)->selected == SEL_POSSIBLE_MOVE);
        
        CPPUNIT_ASSERT(board->getCell(2, 0)->piece);
        
        game.onCellClick(board->getCell(1,0));
        CPPUNIT_ASSERT(board->getCell(1,0)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(2,0)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(1,11)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(1,1)->selected == SEL_NONE);
        
        CPPUNIT_ASSERT(!board->getCell(2, 0)->piece);
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece);
        
        // Move 2
        game.onCellClick(board->getCell(1,0));
        CPPUNIT_ASSERT(board->getCell(1,0)->selected == SEL_CLICKED);
        CPPUNIT_ASSERT(board->getCell(2,0)->selected == SEL_POSSIBLE_MOVE);
        CPPUNIT_ASSERT(board->getCell(0,0)->selected == SEL_POSSIBLE_MOVE);
        CPPUNIT_ASSERT(board->getCell(1,1)->selected == SEL_POSSIBLE_MOVE);
        CPPUNIT_ASSERT(board->getCell(1,11)->selected == SEL_POSSIBLE_MOVE);
        
        CPPUNIT_ASSERT(!board->getCell(2, 0)->piece);
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece);
                
        game.onCellClick(board->getCell(2,0));
        CPPUNIT_ASSERT(board->getCell(1,0)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(2,0)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(0,0)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(1,1)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(1,11)->selected == SEL_NONE);;
        
        CPPUNIT_ASSERT(board->getCell(2, 0)->piece);
        CPPUNIT_ASSERT(!board->getCell(1, 0)->piece);
                
        // Now another player must go
        game.onCellClick(board->getCell(2,0));
        CPPUNIT_ASSERT(board->getCell(2,0)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(1,0)->selected == SEL_NONE);
        CPPUNIT_ASSERT(board->getCell(2,11)->selected == SEL_NONE);     
        
        TRY_CATCH;
    }
    
    
    void testMove2()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Game game;
        game.startup();
        
        BoardPtr board = game.getBoard();
        board->getCell(2, 0)->piece->moveRating = 2;   
        
        game.onCellClick(board->getCell(2,0)); // ally
        
        CellPtr cell26 = board->getCell(2,6);
        game.onCellClick(cell26); // enemy

        CellPtr cell20 = board->getCell(2,0);
        game.onCellClick(cell20); // ally
        
        CellPtr cell00 = board->getCell(0,0);
        game.onCellClick(cell00); // move
        
        CPPUNIT_ASSERT(!board->getCell(2, 0)->piece);
        CPPUNIT_ASSERT(board->getCell(0, 0)->piece);
        
        TRY_CATCH;
    }
    
    void testSelection() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Game game;
        game.startup();
        
        BoardPtr board = game.getBoard();
        board->getCell(2, 0)->piece->moveRating = 1;
        
        game.onCellClick(board->getCell(2,0));
        CPPUNIT_ASSERT(board->getCell(2,0)->selected == SEL_CLICKED);
        CPPUNIT_ASSERT(board->getCell(1,0)->selected == SEL_POSSIBLE_MOVE);
        CPPUNIT_ASSERT(board->getCell(2,11)->selected == SEL_POSSIBLE_MOVE);

        game.onCellClick(board->getCell(2,3));
        CPPUNIT_ASSERT(board->getCell(2,3)->selected == SEL_CLICKED);
        CPPUNIT_ASSERT(board->getCell(1,3)->selected == SEL_POSSIBLE_MOVE);
        CPPUNIT_ASSERT(board->getCell(2,4)->selected == SEL_POSSIBLE_MOVE);

        game.onCellClick(board->getCell(2,0));
        CPPUNIT_ASSERT(board->getCell(2,0)->selected == SEL_CLICKED);
        CPPUNIT_ASSERT(board->getCell(1,0)->selected == SEL_POSSIBLE_MOVE);
        CPPUNIT_ASSERT(board->getCell(2,11)->selected == SEL_POSSIBLE_MOVE);

        game.onCellClick(board->getCell(2,3));
        CPPUNIT_ASSERT(board->getCell(2,3)->selected == SEL_CLICKED);
        CPPUNIT_ASSERT(board->getCell(1,3)->selected == SEL_POSSIBLE_MOVE);
        CPPUNIT_ASSERT(board->getCell(2,4)->selected == SEL_POSSIBLE_MOVE);
        
        game.onCellClick(board->getCell(2,0));
        CPPUNIT_ASSERT(board->getCell(2,0)->selected == SEL_CLICKED);
        CPPUNIT_ASSERT(board->getCell(1,0)->selected == SEL_POSSIBLE_MOVE);
        CPPUNIT_ASSERT(board->getCell(2,11)->selected == SEL_POSSIBLE_MOVE);

        game.onCellClick(board->getCell(2,3));
        CPPUNIT_ASSERT(board->getCell(2,3)->selected == SEL_CLICKED);
        CPPUNIT_ASSERT(board->getCell(1,3)->selected == SEL_POSSIBLE_MOVE);
        CPPUNIT_ASSERT(board->getCell(2,4)->selected == SEL_POSSIBLE_MOVE);
        
        TRY_CATCH;
    }
    

    void testStart() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Game game;
        game.startup();
        
        CPPUNIT_ASSERT_EQUAL(4, (int)game.mPlayer1->howManyPieces());
        CPPUNIT_ASSERT_EQUAL(4, (int)game.mPlayer2->howManyPieces());
        
        // players have different pieces
        for(unsigned i = 0; i < game.mPlayer1->mPieces.size(); i++)
        {
            game.mPlayer1->mPieces[i];
            
            bool res = (find(game.mPlayer2->mPieces.begin(), 
                            game.mPlayer2->mPieces.end(), 
                            game.mPlayer1->mPieces[i]) == game.mPlayer2->mPieces.end());
            
            CPPUNIT_ASSERT(res == true); // not found
        }
        
        //pieces have proper positions
        CPPUNIT_ASSERT(!game.mBoard->getCell(0,0)->piece);
        
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,0)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,1)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,2)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,3)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,4)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,5)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,6)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,7)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,8)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,9)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,10)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(1,11)->piece);
        
        CPPUNIT_ASSERT(game.mBoard->getCell(2,0)->piece);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,1)->piece);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,2)->piece);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,3)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(2,4)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(2,5)->piece);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,6)->piece);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,7)->piece);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,8)->piece);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,9)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(2,10)->piece);
        CPPUNIT_ASSERT(!game.mBoard->getCell(2,11)->piece);
        
        CPPUNIT_ASSERT(game.mBoard->getCell(2,0)->piece->player == game.mPlayer1);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,1)->piece->player == game.mPlayer1);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,2)->piece->player == game.mPlayer1);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,3)->piece->player == game.mPlayer1);
        
        CPPUNIT_ASSERT(game.mBoard->getCell(2,6)->piece->player == game.mPlayer2);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,7)->piece->player == game.mPlayer2);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,8)->piece->player == game.mPlayer2);
        CPPUNIT_ASSERT(game.mBoard->getCell(2,9)->piece->player == game.mPlayer2);  
        
        TRY_CATCH;
    }
    

    
        
    void testCheckVictory() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Game game;
        game.startup();   
        
        PlayerPtr vinner;
        bool res = game.checkVictory(vinner);
        
        CPPUNIT_ASSERT(res == false);
        
        // clear all
        PiecePtr piece0 = game.mPlayer1->mPieces[0];
        PiecePtr piece1 = game.mPlayer1->mPieces[1];
        PiecePtr piece2 = game.mPlayer1->mPieces[2];
        PiecePtr piece3 = game.mPlayer1->mPieces[3];
        
        game.mBoard->killPiece(piece0);
        game.mBoard->killPiece(piece1);
        game.mBoard->killPiece(piece2);
        game.mBoard->killPiece(piece3);
        
        CPPUNIT_ASSERT_EQUAL(0, (int)game.mPlayer1->howManyPieces());
        CPPUNIT_ASSERT_EQUAL(4, (int)game.mPlayer2->howManyPieces());


        res = game.checkVictory(vinner);
        
        CPPUNIT_ASSERT(res == true);
        CPPUNIT_ASSERT(vinner == game.mPlayer2);
        
        TRY_CATCH;
    }
    
    void testOver()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Game game;
        game.startup();   
        
        CPPUNIT_ASSERT(game.isOver() == false);
        
        // clear all
        PiecePtr piece0 = game.mPlayer1->mPieces[0];
        PiecePtr piece1 = game.mPlayer1->mPieces[1];
        PiecePtr piece2 = game.mPlayer1->mPieces[2];
        PiecePtr piece3 = game.mPlayer1->mPieces[3];
        
        game.mBoard->killPiece(piece0);
        game.mBoard->killPiece(piece1);
        game.mBoard->killPiece(piece2);
        game.mBoard->killPiece(piece3);

        CPPUNIT_ASSERT(game.isOver() == true);

        
        TRY_CATCH;
    }


};


CPPUNIT_TEST_SUITE_REGISTRATION(TestGame); 
   
#endif /*TestGameH_*/
