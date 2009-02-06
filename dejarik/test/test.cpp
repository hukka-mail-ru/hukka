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
   CPPUNIT_TEST(testIsMoveValid);
   CPPUNIT_TEST_SUITE_END();
         
   
public:         
    void setUp() {}
    void tearDown() {}
        

    void testBoard() 
    {
        try
        {
        Board board;
                
        CPPUNIT_ASSERT_EQUAL(25, (int)board.cells.size());
        
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
        
        CPPUNIT_ASSERT(board.isMoveValid(king, board.getCell(0,0)) == false); // no move
        CPPUNIT_ASSERT(board.isMoveValid(king, board.getCell(1,5)) == true);
        CPPUNIT_ASSERT(board.isMoveValid(king, board.getCell(2,11)) == false);

        // ----------------------------------------
        // from outer circle to center
        CellPtr outer = board.getCell(2,0);

        // Queen can go 2 calls
        PiecePtr queen (new Piece("Queen", outer, 0, 0, 2));
        outer->piece = queen;

        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(2,0)) == false); // no move
        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(2,10)) == true);
        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(1,11)) == true);
        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(0,0)) == false); // we have King there!
        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(1,1)) == true);
        
        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(1,2)) == false);
        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(2,9)) == false); // out of possible moves
        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(1,10)) == false);
        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(1,5)) == false);
        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(1,2)) == false);
        CPPUNIT_ASSERT(board.isMoveValid(queen, board.getCell(2,3)) == false);
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
