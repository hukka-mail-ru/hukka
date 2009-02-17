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
   CPPUNIT_TEST(testOnCellClick);
   CPPUNIT_TEST(testCheckVictory);
   CPPUNIT_TEST(testOver);
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
        

    void testStart() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Game game;
        game.start();
        
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
        
        TRY_CATCH;
    }
    
    void testOnCellClick() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME; 
        
        TestPiecesMoveOneCell = true;
    
        Game game;
        game.start();
        
        PiecePtr mine = game.mBoard->getCell(2,0)->piece;
        PiecePtr enemy = game.mBoard->getCell(2,9)->piece;
        
        // player1 - me
        CPPUNIT_ASSERT(game.onCellClick(2,9) == false); // click on enemy cell
        CPPUNIT_ASSERT(game.onCellClick(0,0) == false); // click on empty cell
        CPPUNIT_ASSERT(game.onCellClick(2,0) == true);  // select 'mine'
        CPPUNIT_ASSERT(game.onCellClick(2,5) == false); // wrong move
        CPPUNIT_ASSERT(game.onCellClick(1,0) == true);  // move 'mine'
        CPPUNIT_ASSERT(game.onCellClick(1,0) == true);  // select 'mine'
        CPPUNIT_ASSERT(game.onCellClick(1,11) == true); // move 'mine'
        
        // player2 -enemy
        CPPUNIT_ASSERT(game.onCellClick(2,3) == false); // click on my cell
        CPPUNIT_ASSERT(game.onCellClick(0,0) == false); // click on empty cell
        CPPUNIT_ASSERT(game.onCellClick(2,9) == true);  // select 'enemy'
        CPPUNIT_ASSERT(game.onCellClick(2,4) == false); // wrong move
        CPPUNIT_ASSERT(game.onCellClick(2,10) == true);  // move 'enemy'
        CPPUNIT_ASSERT(game.onCellClick(2,10) == true);  // select 'enemy'
        CPPUNIT_ASSERT(game.onCellClick(1,10) == true); // move 'enemy'
        
        // player1 - me
        
        TestPiecesMoveOneCell = false;
        
        TRY_CATCH;
    }
    
        
    void testCheckVictory() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Game game;
        game.start();   
        
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
        game.start();   
        
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
