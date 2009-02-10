#ifndef TESTBOARD_H_
#define TESTBOARD_H_

#include <cppunit/extensions/HelperMacros.h>

#include <string>

#include "Board.h"
#include "Cell.h"
#include "Piece.h"
#include "Macros.h"

class TestBoard : public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestBoard);
   CPPUNIT_TEST(testBoard);
   CPPUNIT_TEST(testPossibleMoves);
   CPPUNIT_TEST(testPossibleTargets);
   CPPUNIT_TEST(testIsMoveValid);
   CPPUNIT_TEST(testGetMoveSteps);
   CPPUNIT_TEST_SUITE_END();
         
   
public:         
    void setUp() {}
    void tearDown() {}
        

    void testBoard() 
    {
        TRY_BEGINS; 
        
        Board board;
                
        CPPUNIT_ASSERT_EQUAL(25, (int)board.mCells.size());
        
        CPPUNIT_ASSERT_THROW(board.getCell(-1,-1), std::string); // both negative
        CPPUNIT_ASSERT_THROW(board.getCell(-1,0), std::string);  // one negative
        CPPUNIT_ASSERT_THROW(board.getCell(0,-1), std::string);  // one negative
        
        CPPUNIT_ASSERT_THROW(board.getCell(0,1), std::string); // out of bound
        CPPUNIT_ASSERT_THROW(board.getCell(3,0), std::string);  // out of bound
        CPPUNIT_ASSERT_THROW(board.getCell(2,12), std::string); // out of bound
        
        CPPUNIT_ASSERT_NO_THROW(board.getCell(0,0)); // normal
        CPPUNIT_ASSERT_NO_THROW(board.getCell(1,0)); // normal
        CPPUNIT_ASSERT_NO_THROW(board.getCell(2,0)); // normal
        
        CPPUNIT_ASSERT_NO_THROW(board.getCell(1,11)); // normal
        CPPUNIT_ASSERT_NO_THROW(board.getCell(2,11)); // normal
        
        CATCH("testBoard");

    }
    
    // helper for testPossibleMoves
    bool findCell(std::vector<CellPtr> moves, unsigned c, unsigned x)
    {      
        for(unsigned i=0; i<moves.size(); ++i)
        {
            if(moves[i]->c == c && moves[i]->x == x)
            {
                return true;
            }
        }
        
        return false;
    }
    
    void testPossibleMoves()
    {
        TRY_BEGINS; 
        
        Board board; 
        
        // King can go 1 cell
        PiecePtr king (new Piece("King", 0, 0, 1));
        board.placePiece(king, 0, 0);
        
        std::vector<CellPtr> moves;
        board.getPossibleMoves(king, moves);
        CPPUNIT_ASSERT_EQUAL((unsigned)12, moves.size());
                
        CPPUNIT_ASSERT(findCell(moves, 1,0) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,1) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,2) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,3) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,4) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,5) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,6) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,7) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,8) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,9) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,10) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,11) == true);
        
        // -------------------------------------------------------------
        board.clear(); 
        king = PiecePtr(new Piece("King", 0, 0, 1));
        board.placePiece(king, 1, 0);
        
        moves.clear();
        board.getPossibleMoves(king, moves);
        CPPUNIT_ASSERT_EQUAL((unsigned)4, moves.size());
                
        CPPUNIT_ASSERT(findCell(moves, 0,0) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,1) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,11) == true);
        CPPUNIT_ASSERT(findCell(moves, 2,0) == true);

        // -------------------------------------------------------------
        board.clear(); 
        // King can go 1 cell
        king = PiecePtr(new Piece("King", 0, 0, 1));
        board.placePiece(king, 2, 0);
        
        moves.clear();
        board.getPossibleMoves(king, moves);
        CPPUNIT_ASSERT_EQUAL((unsigned)3, moves.size());
                
        CPPUNIT_ASSERT(findCell(moves, 2,1) == true);
        CPPUNIT_ASSERT(findCell(moves, 2,11) == true);
        CPPUNIT_ASSERT(findCell(moves, 1,0) == true);
        
        // -------------------------------------------------------------
        board.clear(); 
        // King can go 1 cell
        king = PiecePtr(new Piece("King", 0, 0, 1));
        board.placePiece(king, 2, 0);
        
        PiecePtr queen1 (new Piece("Queen", 0, 0, 1));
        PiecePtr queen2 (new Piece("Queen", 0, 0, 1));
        PiecePtr queen3 (new Piece("Queen", 0, 0, 1));
        
        board.placePiece(queen1, 2, 1);
        board.placePiece(queen2, 2, 11);
        board.placePiece(queen3, 1, 0);

        moves.clear();
        board.getPossibleMoves(king, moves);
        CPPUNIT_ASSERT_EQUAL((unsigned)0, moves.size());
        
        CATCH("testPossibleMoves");
    }
    
    void testPossibleTargets()
    {
        TRY_BEGINS; 
        
        Board board; 
        
        // Two players
        PlayerPtr player1 (new Player());
        PlayerPtr player2 (new Player());
        
        // King can go 1 cell
        PiecePtr king (new Piece("King", 0, 0, 1));
        board.placePiece(king, 0, 0);
        king->setPlayer(player1);
        
        PiecePtr queen1 (new Piece("Queen", 0, 0, 1));
        PiecePtr queen2 (new Piece("Queen", 0, 0, 1));
        PiecePtr queen3 (new Piece("Queen", 0, 0, 1));
        PiecePtr queen4 (new Piece("Queen", 0, 0, 1));
        board.placePiece(queen1, 1, 5);
        board.placePiece(queen2, 1, 10);
        board.placePiece(queen3, 1, 11);
        board.placePiece(queen4, 2, 0); 
        queen1->setPlayer(player1); // ally
        queen2->setPlayer(player2); // ok
        queen3->setPlayer(player2); // ok
        queen4->setPlayer(player2); // out of range
        
        std::vector<CellPtr> targets;
        board.getPossibleTargets(king, targets);
        CPPUNIT_ASSERT_EQUAL((unsigned)2, targets.size());
        
        CPPUNIT_ASSERT(findCell(targets, 1,10) == true);
        CPPUNIT_ASSERT(findCell(targets, 1,11) == true);
        
        CATCH("testPossibleTargets");
    }
    
    void testIsMoveValid() 
    {
        TRY_BEGINS ;
        
        Board board;

        // King can go 1 cell
        PiecePtr king (new Piece("King", 0, 0, 1));
        board.placePiece(king, 0, 0);
        
        std::vector<CellPtr> moves;
        board.getPossibleMoves(king, moves);
        
        CPPUNIT_ASSERT(board.isMoveValid(0,0) == false); // no move
        CPPUNIT_ASSERT(board.isMoveValid(1,5) == true);
        CPPUNIT_ASSERT(board.isMoveValid(2,11) == false);

        // ----------------------------------------
        
        // Queen can go 2 calls
        PiecePtr queen (new Piece("Queen", 0, 0, 2));
        board.placePiece(queen, 2, 0); // from outer circle to center
        
        moves.clear();
        board.getPossibleMoves(queen, moves);

        CPPUNIT_ASSERT(board.isMoveValid(2,0) == false); // no move
        CPPUNIT_ASSERT(board.isMoveValid(2,10) == true);
        CPPUNIT_ASSERT(board.isMoveValid(1,11) == true);
        CPPUNIT_ASSERT(board.isMoveValid(0,0) == false); // we have King there!
        CPPUNIT_ASSERT(board.isMoveValid(1,1) == true);
        
        CPPUNIT_ASSERT(board.isMoveValid(1,2) == false);
        CPPUNIT_ASSERT(board.isMoveValid(2,9) == false); // out of possible moves
        CPPUNIT_ASSERT(board.isMoveValid(1,10) == false);
        CPPUNIT_ASSERT(board.isMoveValid(1,5) == false);
        CPPUNIT_ASSERT(board.isMoveValid(1,2) == false);
        CPPUNIT_ASSERT(board.isMoveValid(2,3) == false);
        
        CATCH("testIsMoveValid");
    }
    
    void testGetMoveSteps() 
    {
        TRY_BEGINS ;
        
        Board board;
        
        // ----------------------------------------
        // from outer circle to center
        CellPtr start = board.getCell(2,0);
        CellPtr interim = board.getCell(1,0);
        CellPtr finish = board.getCell(0,0);

        // Queen can go 2 calls
        PiecePtr queen (new Piece("Queen", 0, 0, 2));
        board.placePiece(queen, 2, 0);
        
        std::vector<CellPtr> moves;
        board.getPossibleMoves(queen, moves);
        
        std::vector<CellPtr> steps;
        board.getMoveSteps(0, 0, steps);
        
        CPPUNIT_ASSERT_EQUAL((unsigned)2, steps.size());
        
        CPPUNIT_ASSERT_EQUAL(interim->c, steps[0]->c);
        CPPUNIT_ASSERT_EQUAL(interim->x, steps[0]->x);
        
        CPPUNIT_ASSERT_EQUAL(finish->c, steps[1]->c);
        CPPUNIT_ASSERT_EQUAL(finish->x, steps[1]->x);
        
        // --------------------------------------------------------------
        steps.clear();
        board.getMoveSteps(2, 2, steps);        
        CPPUNIT_ASSERT_EQUAL((unsigned)2, steps.size());
        
        steps.clear();
        board.getMoveSteps(1, 1, steps);        
        CPPUNIT_ASSERT_EQUAL((unsigned)2, steps.size());

        steps.clear();
        board.getMoveSteps(1, 11, steps);        
        CPPUNIT_ASSERT_EQUAL((unsigned)2, steps.size());

        CATCH("testGetMoveSteps");
    }
    
};


CPPUNIT_TEST_SUITE_REGISTRATION(TestBoard); 

#endif /*TESTBOARD_H_*/
