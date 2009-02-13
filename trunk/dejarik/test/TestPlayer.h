
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
   
   CPPUNIT_TEST(testGetBattleResult);
   
   CPPUNIT_TEST(testMakePush);
   CPPUNIT_TEST(testMakeCounterPush);
   
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
    
    void testMakePush()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        TestMakePush = true; // globally
        
        BoardPtr board (new Board);    
        PiecePtr mine (new Piece("White King", 7, 0, 1)); // ] attack rating is enough for push 
        PiecePtr enemy (new Piece("Black King", 0, 2, 1)); 
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(mine, 1, 0);
        board->placePiece(enemy, 1, 1);
        
        board->distribute(mine, player1);
        board->distribute(enemy, player2);
        
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == mine); // we are here
        CPPUNIT_ASSERT(board->getCell(1, 1)->piece == enemy); // enemy is here
                
        BattleResult battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START, battleRes) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START, battleRes) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_FINISH, battleRes) == true); // attack

        CPPUNIT_ASSERT(battleRes == RES_PUSH);
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == mine); // we are still here
        CPPUNIT_ASSERT(board->getCell(1, 1)->piece == enemy); // enemy is still here
        
        CPPUNIT_ASSERT(player1->makePush(2, 5) == false); // click out of range
        CPPUNIT_ASSERT(player1->makePush(1, 5) == false); // click out of range
        CPPUNIT_ASSERT(player1->makePush(1, 2) == true); // push

        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == mine); // we are still here
        CPPUNIT_ASSERT(board->getCell(1, 2)->piece == enemy); // enemy pushed

        
        TestMakePush = false; // globally
        
        TRY_CATCH;
    }
    
    void testMakeCounterPush()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        TestMakeCounterPush = true; // globally
        
        BoardPtr board (new Board);    
        PiecePtr mine (new Piece("White King", 2, 0, 1));  
        PiecePtr enemy (new Piece("Black King", 0, 7, 1)); // ] defence rating is enough for push 
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(mine, 1, 0);
        board->placePiece(enemy, 1, 1);
        
        board->distribute(mine, player1);
        board->distribute(enemy, player2);
        
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == mine); // we are here
        CPPUNIT_ASSERT(board->getCell(1, 1)->piece == enemy); // enemy is here
                
        BattleResult battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START, battleRes) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START, battleRes) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_FINISH, battleRes) == true); // attack

        CPPUNIT_ASSERT(battleRes == RES_COUNTER_PUSH);
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == mine); // we are still here
        CPPUNIT_ASSERT(board->getCell(1, 1)->piece == enemy); // enemy is still here
        
        CPPUNIT_ASSERT(player2->makePush(2, 5) == false); // click out of range
        CPPUNIT_ASSERT(player2->makePush(1, 5) == false); // click out of range
        CPPUNIT_ASSERT(player2->makePush(1, 11) == true); // push

        CPPUNIT_ASSERT(board->getCell(1, 11)->piece == mine); // pushed
        CPPUNIT_ASSERT(board->getCell(1, 1)->piece == enemy); // enemy is still here
        
        TestMakeCounterPush = false; // globally   
        
        TRY_CATCH;
    }
    
    void testGetBattleResult()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);  
        PlayerPtr player (new Player("default", board));
        
        BattleResult res = player->getBattleResult(0, 0);
        CPPUNIT_ASSERT(res == RES_COUNTER_PUSH);

        res = player->getBattleResult(0, 0);
        CPPUNIT_ASSERT(res == RES_COUNTER_PUSH);

        res = player->getBattleResult(2, 1);
        CPPUNIT_ASSERT(res == RES_KILL || res == RES_PUSH || res == RES_COUNTER_PUSH);

        res = player->getBattleResult(3, 2);
        CPPUNIT_ASSERT(res == RES_KILL || res == RES_PUSH ||
                res == RES_COUNTER_KILL || res == RES_COUNTER_PUSH);
        
        res = player->getBattleResult(1, 2);
        CPPUNIT_ASSERT(res == RES_COUNTER_KILL || res == RES_COUNTER_PUSH || res == RES_PUSH);
        
        TRY_CATCH;
    }
    
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
        
        CPPUNIT_ASSERT(player1->attackEnimy(mine, enemy) == RES_KILL);
        
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
        
        CPPUNIT_ASSERT(player1->attackEnimy(mine, enemy) == RES_COUNTER_KILL);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 0);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
        
        TRY_CATCH;
    }
    
    void testAttackEnimy_push()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);    
        PiecePtr mine (new Piece("White King", 1, 0, 1)); // attack rating is 1 
        PiecePtr enemy (new Piece("Black King", 0, 0, 1)); // defence rating is 0 
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(mine, 1, 0);
        board->placePiece(enemy, 1, 1);
        
        board->distribute(mine, player1);
        board->distribute(enemy, player2);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);

        CPPUNIT_ASSERT(player1->attackEnimy(mine, enemy) == RES_PUSH);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);        
        
        TRY_CATCH; 
    }
    
    void testAttackEnimy_counterPush()
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        BoardPtr board (new Board);    
        PiecePtr mine (new Piece("White King", 0, 0, 1)); // attack rating is 0 
        PiecePtr enemy (new Piece("Black King", 0, 1, 1)); // defence rating is 1 
        PlayerPtr player1 (new Player("default", board));
        PlayerPtr player2 (new Player("default", board));
        
        board->placePiece(mine, 1, 0);
        board->placePiece(enemy, 1, 1);
        
        board->distribute(mine, player1);
        board->distribute(enemy, player2);

        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
        CPPUNIT_ASSERT(player1->attackEnimy(mine, enemy) == RES_COUNTER_PUSH);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
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
        
        CPPUNIT_ASSERT(player1->attackEnimy(mine, enemy) == RES_COUNTER_PUSH);
        
        CPPUNIT_ASSERT(player1->howManyPieces() == 1);         
        CPPUNIT_ASSERT(player2->howManyPieces() == 1);
        
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
        
        BattleResult battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START, battleRes) == true);  // click on the piece
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 10, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_FINISH, battleRes) == true); // click in range. move there

        CPPUNIT_ASSERT(battleRes == RES_NO_BATTLE);
        CPPUNIT_ASSERT(board->getCell(1, 11)->piece == king1); // then we are here
        
        battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_START, battleRes) == true); // click on the piece
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_FINISH, battleRes) == true); // click in range. move there
        
        CPPUNIT_ASSERT(battleRes == RES_NO_BATTLE);
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
        
        BattleResult battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START, battleRes) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START, battleRes) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_FINISH, battleRes) == true); // attack

        CPPUNIT_ASSERT(battleRes != RES_NO_BATTLE);
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are still here
        
        battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START, battleRes) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START, battleRes) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_FINISH, battleRes) == true); // attack
        
        CPPUNIT_ASSERT(battleRes != RES_NO_BATTLE);
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
        
        BattleResult battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START, battleRes) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START, battleRes) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_FINISH, battleRes) == true); // attack

        CPPUNIT_ASSERT(battleRes != RES_NO_BATTLE);
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king1); // we are still here
        
        battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 1, TURN_START, battleRes) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START, battleRes) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_FINISH, battleRes) == true); // move
        
        CPPUNIT_ASSERT(battleRes == RES_NO_BATTLE);
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

        BattleResult battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START, battleRes) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START, battleRes) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_FINISH, battleRes) == true); // click in range. move there

        CPPUNIT_ASSERT(battleRes == RES_NO_BATTLE);
        CPPUNIT_ASSERT(board->getCell(1, 11)->piece == king1); // then we are here
        
        battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START, battleRes) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(1, 11, TURN_START, battleRes) == true); // click on ally
        
        CPPUNIT_ASSERT(player1->makeTurn(2, 7, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 4, TURN_FINISH, battleRes) == false); // click out of range
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_FINISH, battleRes) == true); // click in range. attack
        
        CPPUNIT_ASSERT(battleRes != RES_NO_BATTLE);
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
        
        BattleResult battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_START, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 0, TURN_START, battleRes) == true); // click on king
        
        CPPUNIT_ASSERT(player1->makeTurn(1, 5, TURN_FINISH, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(1, 10, TURN_FINISH, battleRes) == false); // click on empty cell
        CPPUNIT_ASSERT(player1->makeTurn(2, 0, TURN_FINISH, battleRes) == true); // click on slon
        
        CPPUNIT_ASSERT(battleRes == RES_NO_BATTLE);
        
        battleRes = RES_NO_BATTLE;
        CPPUNIT_ASSERT(player1->makeTurn(0, 0, TURN_FINISH, battleRes) == false); // out of range
        CPPUNIT_ASSERT(player1->makeTurn(2, 5, TURN_FINISH, battleRes) == false); // click on enemy
        CPPUNIT_ASSERT(player1->makeTurn(2, 1, TURN_FINISH, battleRes) == true); // move
        
        CPPUNIT_ASSERT(battleRes == RES_NO_BATTLE);
        CPPUNIT_ASSERT(board->getCell(1, 0)->piece == king); // we didn't move
        CPPUNIT_ASSERT(board->getCell(2, 1)->piece == slon); // we moved
        
        TRY_CATCH
    }
    
    


};


CPPUNIT_TEST_SUITE_REGISTRATION(TestPlayer); 
   
#endif /*TESTPLAYER_H_*/
