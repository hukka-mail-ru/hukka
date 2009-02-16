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
        
    void testCheckVictory() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Game game;
        game.start();         
        
        // clear all
        for(unsigned i = 0; i < game.mPlayer1->mPieces.size(); i++)
        {
            game.mPlayer1->removePiece(game.mPlayer1->mPieces[i]);
        }
        
        CPPUNIT_ASSERT_EQUAL(0, (int)game.mPlayer1->howManyPieces());
        CPPUNIT_ASSERT_EQUAL(4, (int)game.mPlayer2->howManyPieces());

        PlayerPtr vinner;
        bool res = game.checkVictory(vinner);
        
        CPPUNIT_ASSERT(res == true);
        CPPUNIT_ASSERT(vinner == game.mPlayer2);
        
        TRY_CATCH;
    }


};


CPPUNIT_TEST_SUITE_REGISTRATION(TestGame); 
   
#endif /*TestGameH_*/
