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

#include "../src/Board.h"
#include "../src/Cell.h"
#include "../src/Piece.h"

using namespace std;

class DejaricTest : public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(DejaricTest);
   CPPUNIT_TEST(testOne);
   CPPUNIT_TEST(testIsMoveValid);
   CPPUNIT_TEST_SUITE_END();
         
   
public:         
    void setUp() {}
    void tearDown() {}
        

    void testOne() 
    {
        Board board;
                
        CPPUNIT_ASSERT_EQUAL(25, (int)board.cells.size());
        
        /*
        try
        {
            board.getCell(10,10);
        }
        catch(string& err)
        {
            cerr << err << endl;
        }*/
        
        CPPUNIT_ASSERT_NO_THROW(board.getCell(0,0));
        CPPUNIT_ASSERT_THROW(board.getCell(10,10), string);
    }
    
    
    void testIsMoveValid() 
    {
        Board board;
        
        CellPtr start = board.getCell(0,0);
        CellPtr finish = board.getCell(0,0);
        
        PiecePtr piece (new Piece);
        piece->setPosition(start);

        CPPUNIT_ASSERT(board.isMoveValid(piece, finish) == true);
        
        finish = board.getCell(2,10);
        CPPUNIT_ASSERT(board.isMoveValid(piece, finish) == true);

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
