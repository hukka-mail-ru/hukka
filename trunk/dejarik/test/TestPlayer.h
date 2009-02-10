#ifndef TESTPLAYER_H_
#define TESTPLAYER_H_


#include <cppunit/extensions/HelperMacros.h>

#include "Player.h"
#include "Macros.h"

class TestPlayer: public CppUnit::TestFixture 
{ 
   /* during a turn, 
    * a player may move a piece twice, or may attack two other pieces, 
    * or move and then attack, or attack and then move
    */
   CPPUNIT_TEST_SUITE(TestPlayer);
   CPPUNIT_TEST(testMakeTurn_moveTwice);
   CPPUNIT_TEST(testMakeTurn_attackTwice);
   CPPUNIT_TEST(testMakeTurn_moveThenAttack);
   CPPUNIT_TEST(testMakeTurn_attackThenMove);   
   
   CPPUNIT_TEST(testMakeTurn_selectAnotherPiece);
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
    
    void testMakeTurn_attackTwice() 
    {
        TRY_BEGINS;
        
        Board board;        
        PiecePtr king1 (new Piece("White King", 0, 0, 1)); // king can go 1 cell
        PiecePtr king2 (new Piece("Black King", 0, 0, 1)); // king can go 1 cell
        
        board.placePiece(king1, 1, 0);
        board.placePiece(king2, 1, 10);
        
        PlayerPtr player1 (new Player);
        PlayerPtr player2 (new Player);

        player1->addPiece(king1);
        player2->addPiece(king2);
        
        unsigned click = 1;       
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, click) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, click) == false); // click on enimy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, click) == true); // click on ally
        
        TRY_CATCH
    }
    
    void testMakeTurn_attackThenMove() 
    {
    }
    
    void testMakeTurn_selectAnotherPiece() 
    {
    }
    
        
    void testMakeTurn_moveTwice() 
    {
        TRY_BEGINS;
        
        Board board;        
        PiecePtr king1 (new Piece("White King", 0, 0, 1)); // king can go 1 cell        
        board.placePiece(king1, 1, 0);
        
        PlayerPtr player1 (new Player);
        player1->addPiece(king1);
        
        unsigned click = 1;       
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, click) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, click) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, click) == true);  // click on the piece
        
        click = 2;
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, click) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 10, click) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, click) == true); // click in range. move there

        click = 3;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, click) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, click) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, click) == true); // click on ally
        
        click = 4;
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, click) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 10, click) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 11, click) == true); // click in range. move there
        
        CPPUNIT_ASSERT(board.getCell(0, 11)->piece == king1); // we are here
        
        click = 5;
        CPPUNIT_ASSERT_THROW(player1->makeTurn(0, 11, click), std::string);
        
        TRY_CATCH
    }
    
    void testMakeTurn_moveThenAttack() 
    {
        TRY_BEGINS;
        
        Board board;        
        PiecePtr king1 (new Piece("White King", 0, 0, 1)); // king can go 1 cell
        PiecePtr king2 (new Piece("Black King", 0, 0, 1)); // king can go 1 cell
        
        board.placePiece(king1, 1, 0);
        board.placePiece(king2, 1, 10);
        
        PlayerPtr player1 (new Player);
        PlayerPtr player2 (new Player);

        player1->addPiece(king1);
        player2->addPiece(king2);
        
        unsigned click = 1;       
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, click) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, click) == false); // click on enimy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, click) == true); // click on ally
        
        click = 2;
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, click) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 10, click) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, click) == true); // click in range. move there

        click = 3;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, click) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, click) == false); // click on enimy
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, click) == true); // click on ally
        
        click = 4;
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, click) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 10, click) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, click) == true); // click in range. attack
        
        CPPUNIT_ASSERT(board.getCell(1, 11)->piece == king1); // we are here
        
        TRY_CATCH
    }
    
    
    
    


};


CPPUNIT_TEST_SUITE_REGISTRATION(TestPlayer); 
   
#endif /*TESTPLAYER_H_*/
