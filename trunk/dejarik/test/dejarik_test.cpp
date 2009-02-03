#include <iostream>
#include <string>
#include <cstdlib>

#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/XmlOutputter.h>
#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/portability/Stream.h>

#include "../src/Board.h"

using namespace std;

class DejaricTest : public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(DejaricTest);
   CPPUNIT_TEST(testOne);
   CPPUNIT_TEST_SUITE_END();
         
   
public:         
    void setUp() {}
    void tearDown() {}
        

    void testOne() 
    {
        Board board;
                
        cerr << "board.cells.size() " << board.cells.size() << endl;
        CPPUNIT_ASSERT(board.cells.size() == 25);
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
