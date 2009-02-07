#include <iostream>
#include <string>
#include <cstdlib>

#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/XmlOutputter.h>
#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/portability/Stream.h>
#include <cppunit/TestAssert.h>

#include "Board.h"
#include "Cell.h"
#include "Piece.h"

using namespace std;

class DejaricTest : public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(DejaricTest);
   CPPUNIT_TEST(testBoard);
   CPPUNIT_TEST(testPossibleMoves);
   CPPUNIT_TEST(testIsMoveValid);
   CPPUNIT_TEST(testGetMoveSteps);
   CPPUNIT_TEST_SUITE_END();
         
   
public:         
    void setUp() {}
    void tearDown() {}
        

    void testBoard() 
    {
        try
        {
        Board board;
                
        CPPUNIT_ASSERT_EQUAL(25, (int)board.mCells.size());
        
        CPPUNIT_ASSERT_THROW(board.getCell(-1,-1), string); // both negative
        CPPUNIT_ASSERT_THROW(board.getCell(-1,0), string);  // one negative
        CPPUNIT_ASSERT_THROW(board.getCell(0,-1), string);  // one negative
        
        CPPUNIT_ASSERT_THROW(board.getCell(0,1), string); // out of bound
        CPPUNIT_ASSERT_THROW(board.getCell(3,0), string);  // out of bound
        CPPUNIT_ASSERT_THROW(board.getCell(2,12), string); // out of bound
        
        CPPUNIT_ASSERT_NO_THROW(board.getCell(0,0)); // normal
        CPPUNIT_ASSERT_NO_THROW(board.getCell(1,0)); // normal
        CPPUNIT_ASSERT_NO_THROW(board.getCell(2,0)); // normal
        
        CPPUNIT_ASSERT_NO_THROW(board.getCell(1,11)); // normal
        CPPUNIT_ASSERT_NO_THROW(board.getCell(2,11)); // normal
        }
        catch(string& err)
        {
            cerr << "testBoard -> " << err << endl;
        }
    }
    
    // helper for testPossibleMoves
    bool findMove(vector<CellPtr> moves, unsigned c, unsigned x)
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
        try
        { 
        Board board; 
        CellPtr cell = board.getCell(0,0);
        
        // King can go 1 cell
        PiecePtr king (new Piece("King", cell, 0, 0, 1));
        cell->piece = king;
        
        vector<CellPtr> moves;
        board.getPossibleMoves(king, moves);
        CPPUNIT_ASSERT_EQUAL((unsigned)12, moves.size());
                
        CPPUNIT_ASSERT(findMove(moves, 1,0) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,1) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,2) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,3) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,4) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,5) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,6) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,7) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,8) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,9) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,10) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,11) == true);
        
        // -------------------------------------------------------------
        board.clear(); 
        cell = board.getCell(1,0);
        
        // King can go 1 cell
        king = PiecePtr(new Piece("King", cell, 0, 0, 1));
        cell->piece = king;
        
        moves.clear();
        board.getPossibleMoves(king, moves);
        CPPUNIT_ASSERT_EQUAL((unsigned)4, moves.size());
                
        CPPUNIT_ASSERT(findMove(moves, 0,0) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,1) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,11) == true);
        CPPUNIT_ASSERT(findMove(moves, 2,0) == true);

        // -------------------------------------------------------------
        board.clear(); 
        cell = board.getCell(2,0);
        
        // King can go 1 cell
        king = PiecePtr(new Piece("King", cell, 0, 0, 1));
        cell->piece = king;
        
        moves.clear();
        board.getPossibleMoves(king, moves);
        CPPUNIT_ASSERT_EQUAL((unsigned)3, moves.size());
                
        CPPUNIT_ASSERT(findMove(moves, 2,1) == true);
        CPPUNIT_ASSERT(findMove(moves, 2,11) == true);
        CPPUNIT_ASSERT(findMove(moves, 1,0) == true);
        
        }
        catch(string& err)
        {
            cerr << "testPossibleMoves -> " << err << endl;
        }
    }
    
    
    void testIsMoveValid() 
    {
        try
        {
        Board board;
        
        // ----------------------------------------
        // start in the center
        CellPtr center = board.getCell(0,0);

        // King can go 1 cell
        PiecePtr king (new Piece("King", center, 0, 0, 1));
        center->piece = king;
        
        vector<CellPtr> moves;
        board.getPossibleMoves(king, moves);
        
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(0,0)) == false); // no move
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(1,5)) == true);
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(2,11)) == false);

        // ----------------------------------------
        // from outer circle to center
        CellPtr outer = board.getCell(2,0);

        // Queen can go 2 calls
        PiecePtr queen (new Piece("Queen", outer, 0, 0, 2));
        outer->piece = queen;
        
        moves.clear();
        board.getPossibleMoves(queen, moves);

        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(2,0)) == false); // no move
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(2,10)) == true);
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(1,11)) == true);
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(0,0)) == false); // we have King there!
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(1,1)) == true);
        
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(1,2)) == false);
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(2,9)) == false); // out of possible moves
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(1,10)) == false);
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(1,5)) == false);
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(1,2)) == false);
        CPPUNIT_ASSERT(board.isMoveValid(board.getCell(2,3)) == false);
        }
        // --------------------------------------------
        catch(string& err)
        {
            cerr << "testIsMoveValid -> " << err << endl;
        }
    
        
    }
    
    void testGetMoveSteps() 
    {
        
        try
        {
        Board board;
        
        // ----------------------------------------
        // from outer circle to center
        CellPtr start = board.getCell(2,0);
        CellPtr interim = board.getCell(1,0);
        CellPtr finish = board.getCell(0,0);

        // Queen can go 2 calls
        PiecePtr queen (new Piece("Queen", start, 0, 0, 2));
        start->piece = queen;
        
        vector<CellPtr> moves;
        board.getPossibleMoves(queen, moves);
        
        vector<CellPtr> steps;
        board.getMoveSteps(finish, steps);
        
        CPPUNIT_ASSERT_EQUAL((unsigned)2, steps.size());
        
        CPPUNIT_ASSERT_EQUAL(interim->c, steps[0]->c);
        CPPUNIT_ASSERT_EQUAL(interim->x, steps[0]->x);
        
        CPPUNIT_ASSERT_EQUAL(finish->c, steps[1]->c);
        CPPUNIT_ASSERT_EQUAL(finish->x, steps[1]->x);
        
        
        finish = board.getCell(2,2);
        steps.clear();
        board.getMoveSteps(finish, steps);        
        CPPUNIT_ASSERT_EQUAL((unsigned)2, steps.size());
        
        finish = board.getCell(2,11);
        steps.clear();
        board.getMoveSteps(finish, steps);        
        CPPUNIT_ASSERT_EQUAL((unsigned)1, steps.size());
        
        finish = board.getCell(1,1);
        steps.clear();
        board.getMoveSteps(finish, steps);        
        CPPUNIT_ASSERT_EQUAL((unsigned)2, steps.size());

        finish = board.getCell(1,11);
        steps.clear();
        board.getMoveSteps(finish, steps);        
        CPPUNIT_ASSERT_EQUAL((unsigned)2, steps.size());

        }
        // --------------------------------------------
        catch(string& err)
        {
            cerr << "testIsMoveValid -> " << err << endl;
        }
        
    }
    
};


CPPUNIT_TEST_SUITE_REGISTRATION(DejaricTest); 


int main()
{
   CppUnit::Test *suite = CppUnit::TestFactoryRegistry::getRegistry().makeTest();
   CppUnit::TextUi::TestRunner runner;
   runner.addTest(suite);
   runner.setOutputter(new CppUnit::CompilerOutputter(&runner.result(), std::cout));
   bool wasSuccessfull = runner.run();
   if (!wasSuccessfull != 0) std::cout << "Error" << std::endl;
   std::cout << "[OVAL] Cppunit-result ="<< !wasSuccessfull << std::endl;
   return EXIT_SUCCESS;
} 
