
#ifndef TESTPLAYER_H_
#define TESTPLAYER_H_


#include <cppunit/extensions/HelperMacros.h>

#include "Player.h"
#include "Macros.h"
#include "Board.h"

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
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);        
        PiecePtr king1 (new Piece("White King", 0, 0, 1)); // king can go 1 cell
        PiecePtr king2 (new Piece("Black King", 0, 0, 1)); // king can go 1 cell
        
        board->placePiece(king1, 1, 0);
        board->placePiece(king2, 1, 1);
        
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));

        player1->addPiece(king1);
        player2->addPiece(king2);

        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START) == false); // click on enimy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_FINISH) == true); // attack

        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are still here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START) == false); // click on enimy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_FINISH) == true); // attack
        
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are still here
        
        
        TRY_CATCH
    }
    
    void testMakeTurn_attackThenMove() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);        
        PiecePtr king1 (new Piece("White King", 0, 0, 1)); // king can go 1 cell
        PiecePtr king2 (new Piece("Black King", 0, 0, 1)); // king can go 1 cell
        
        board->placePiece(king1, 1, 0);
        board->placePiece(king2, 1, 1);
        
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));

        player1->addPiece(king1);
        player2->addPiece(king2);

        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START) == false); // click on enimy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_FINISH) == true); // attack

        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are still here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START) == false); // click on enimy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_FINISH) == true); // move
        
        CPPUNIT_ASSERT(board->getCell(1, 11)->piece == king1); // we're finally here
        
        
        TRY_CATCH
    }
            
    void testMakeTurn_moveTwice() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);        
        PiecePtr king1 (new Piece("White King", 0, 0, 1)); // king can go 1 cell        
        board->placePiece(king1, 1, 0);
        
        PlayerPtr player1 (new Player("default", board));
        player1->addPiece(king1);
        
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true);  // click on the piece
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 10, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_FINISH) == true); // click in range. move there

        CPPUNIT_ASSERT(board->getCell(1, 11)->piece == king1); // then we are here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_START) == true); // click on the piece
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 10, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 11, TURN_FINISH) == true); // click in range. move there
        
        CPPUNIT_ASSERT(board->getCell(0, 11)->piece == king1); // finally we are here
        
        TRY_CATCH
    }
    
    void testMakeTurn_moveThenAttack() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);        
        PiecePtr king1 (new Piece("White King", 0, 0, 1)); // king can go 1 cell
        PiecePtr king2 (new Piece("Black King", 0, 0, 1)); // king can go 1 cell
        
        board->placePiece(king1, 1, 0);
        board->placePiece(king2, 1, 10);
        
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));

        player1->addPiece(king1);
        player2->addPiece(king2);
        
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are here

        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START) == false); // click on enimy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 10, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_FINISH) == true); // click in range. move there

        CPPUNIT_ASSERT(board->getCell(1, 11)->piece == king1); // then we are here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START) == false); // click on enimy
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 10, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_FINISH) == true); // click in range. attack
        
        CPPUNIT_ASSERT(board->getCell(1, 11)->piece == king1); // we didn't move
        
        TRY_CATCH
    }
    
    
    void testMakeTurn_selectAnotherPiece() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);        
        PiecePtr king (new Piece("White King", 0, 0, 1)); // king can go 1 cell
        PiecePtr slon (new Piece("White Slon", 0, 0, 1)); // king can go 1 cell
        
        board->placePiece(king, 1, 0);
        board->placePiece(slon, 2, 0);
        
        PlayerPtr player1 (new Player("default", board));

        player1->addPiece(king);
        player1->addPiece(slon);
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on king
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_FINISH) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_FINISH) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(2, 0, TURN_FINISH) == true); // click on slon
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_FINISH) == false); // out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click on enimy
        CPPUNIT_ASSERT(player1->makeTurn(2, 1, TURN_FINISH) == true); // move
        
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king); // we didn't move
        CPPUNIT_ASSERT(board->getCell(2, 1)->piece == slon); // we moved
        
        TRY_CATCH
    }
    
    


};


CPPUNIT_TEST_SUITE_REGISTRATION(TestPlayer); 
   
#endif /*TESTPLAYER_H_*/
