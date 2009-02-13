
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
   
   CPPUNIT_TEST(testAttackEnimy_kill);
   CPPUNIT_TEST(testAttackEnimy_counterKill);
   CPPUNIT_TEST(testAttackEnimy_push);
   CPPUNIT_TEST(testAttackEnimy_counterPush);
   CPPUNIT_TEST(testAttackEnimy_aDraw); // counter-push
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
    
    
    void testAttackEnimy_kill()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);    
        PiecePtr mine (new Piece("White King", 7, 0, 1)); // attack rating is 7 
        PiecePtr enemy (new Piece("Black King", 0, 0, 1)); // defence rating is 0 
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(mine, 1, 0);
        board->placePiece(enemy, 1, 1);
        
        board->distribute(mine, player1);
        board->distribute(enemy, player2);

        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
        player1->attackEnimy(mine, enemy);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 0);
        
        
        TRY_CATCH;
    }
    
    void testAttackEnimy_counterKill()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);    
        PiecePtr mine (new Piece("White King", 0, 0, 1)); // attack rating is 0 
        PiecePtr enemy (new Piece("Black King", 0, 7, 1)); // defence rating is 7 
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(mine, 1, 0);
        board->placePiece(enemy, 1, 1);
        
        board->distribute(mine, player1);
        board->distribute(enemy, player2);

        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
        player1->attackEnimy(mine, enemy);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 0);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
        
        TRY_CATCH;
    }
    
    void testAttackEnimy_push()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);    
        PiecePtr mine (new Piece("White King", 6, 0, 1)); // attack rating is 6 
        PiecePtr enemy (new Piece("Black King", 0, 0, 1)); // defence rating is 0 
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(mine, 1, 0);
        board->placePiece(enemy, 1, 1);
        
        board->distribute(mine, player1);
        board->distribute(enemy, player2);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);

        player1->attackEnimy(mine, enemy);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
        // on position
        mine->getPosition() == board->getCell(1, 0);
        
        // pushed somewhere there
        CellPtr possiblePush1 = board->getCell(0, 0);
        CellPtr possiblePush2 = board->getCell(1, 2);
        CellPtr possiblePush3 = board->getCell(2, 1);
        CPPUNIT_ASSERT(enemy->getPosition() == possiblePush1 ||
                       enemy->getPosition() == possiblePush2 ||
                       enemy->getPosition() == possiblePush3);
        ;
        TRY_CATCH; 
    }
    
    void testAttackEnimy_counterPush()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);    
        PiecePtr mine (new Piece("White King", 0, 0, 1)); // attack rating is 0 
        PiecePtr enemy (new Piece("Black King", 0, 6, 1)); // defence rating is 6 
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(mine, 1, 0);
        board->placePiece(enemy, 1, 1);
        
        board->distribute(mine, player1);
        board->distribute(enemy, player2);

        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
        player1->attackEnimy(mine, enemy);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
        // on position
        enemy->getPosition() == board->getCell(1, 1);
        
        // pushed somewhere there
        CellPtr possiblePush1 = board->getCell(0, 0);
        CellPtr possiblePush2 = board->getCell(1, 11);
        CellPtr possiblePush3 = board->getCell(2, 0);
        CPPUNIT_ASSERT(mine->getPosition() == possiblePush1 ||
                       mine->getPosition() == possiblePush2 ||
                        mine->getPosition() == possiblePush3);
        
        TRY_CATCH; 
    }
    
    
    void testAttackEnimy_aDraw()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);    
        PiecePtr mine (new Piece("White King", 0, 0, 1)); // attack rating is 0 
        PiecePtr enemy (new Piece("Black King", 0, 0, 1)); // defence rating is 0
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(mine, 1, 0);
        board->placePiece(enemy, 1, 1);
        
        board->distribute(mine, player1);
        board->distribute(enemy, player2);

        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
        player1->attackEnimy(mine, enemy);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
        // on position
        enemy->getPosition() == board->getCell(1, 1);
        
        // pushed somewhere there
        CellPtr possiblePush1 = board->getCell(0, 0);
        CellPtr possiblePush2 = board->getCell(1, 11);
        CellPtr possiblePush3 = board->getCell(2, 0);
        CPPUNIT_ASSERT(mine->getPosition() == possiblePush1 ||
                       mine->getPosition() == possiblePush2 ||
                        mine->getPosition() == possiblePush3);
        
        TRY_CATCH; 
    }
    
    void testMakeTurn_moveTwice() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);        
        PiecePtr king1 (new Piece("White King", 0, 0, 1)); // king can go 1 cell        
        PlayerPtr player1 (new Player("default", board));
        
        board->placePiece(king1, 1, 0);
        board->distribute(king1, player1);

        
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true);  // click on the piece
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 10, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_FINISH) == true); // click in range. move there

        CPPUNIT_ASSERT(board->getCell(1, 11)->piece == king1); // then we are here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_START) == true); // click on the piece
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_FINISH) == true); // click in range. move there
        
        CPPUNIT_ASSERT(board->getCell(0, 0)->piece == king1); // finally we are here
        
        TRY_CATCH
    }
    
    void testMakeTurn_attackTwice() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);        
        PiecePtr king1 (new Piece("White King", 0, 0, 1)); // king can go 1 cell
        PiecePtr king2 (new Piece("Black King", 0, 0, 1)); // king can go 1 cell
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(king1, 1, 0);
        board->placePiece(king2, 1, 1);
        
        board->distribute(king1, player1);
        board->distribute(king2, player2);

        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_FINISH) == true); // attack

        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are still here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH) == false); // click out of range
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
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(king1, 1, 0);
        board->placePiece(king2, 1, 1);
        
        board->distribute(king1, player1);
        board->distribute(king2, player2);

        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_FINISH) == true); // attack

        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are still here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_FINISH) == true); // move
        
        CPPUNIT_ASSERT(board->getCell(1, 11)->piece == king1); // we're finally here
        
        
        TRY_CATCH
    }
            
    
    void testMakeTurn_moveThenAttack() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);        
        PiecePtr king1 (new Piece("White King", 0, 0, 1)); // king can go 1 cell
        PiecePtr king2 (new Piece("Black King", 0, 0, 1)); // king can go 1 cell        
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(king1, 1, 0);
        board->placePiece(king2, 1, 10);

        board->distribute(king1, player1);
        board->distribute(king2, player2);
        
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are here

        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_FINISH) == true); // click in range. move there

        CPPUNIT_ASSERT(board->getCell(1, 11)->piece == king1); // then we are here
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_START) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, TURN_FINISH) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 4, TURN_FINISH) == false); // click out of range
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
        PlayerPtr player1 (new Player("default", board));
        
        board->placePiece(king, 1, 0);
        board->placePiece(slon, 2, 0);

        board->distribute(king, player1);
        board->distribute(slon, player1);
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START) == true); // click on king
        
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_FINISH) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(2, 0, TURN_FINISH) == true); // click on slon
        
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_FINISH) == false); // out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(2, 1, TURN_FINISH) == true); // move
        
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king); // we didn't move
        CPPUNIT_ASSERT(board->getCell(2, 1)->piece == slon); // we moved
        
        TRY_CATCH
    }
    
    


};


CPPUNIT_TEST_SUITE_REGISTRATION(TestPlayer); 
   
#endif /*TESTPLAYER_H_*/
