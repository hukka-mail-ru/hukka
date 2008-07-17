#include <iostream>
#include <string>
#include <cstdlib>

#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/XmlOutputter.h>
#include <cppunit/extensions/HelperMacros.h>
#include <cppunit/portability/Stream.h>

#include <parser.h>

using namespace std;

class TestParser : public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestParser);
//   CPPUNIT_TEST(testPregMatch);
   CPPUNIT_TEST(testGetVarName);
   CPPUNIT_TEST_SUITE_END();
         
   
public:         
    void setUp() {}
    void tearDown() {}
        

    void testPregMatch() 
    {
        Parser parser;
        vector<string> res;
        parser.preg_match(" *([a-zA-Z]+) *;", "   const   int var ;", res);
        CPPUNIT_ASSERT(res.size() == 1);
        CPPUNIT_ASSERT(res[0] == "var");

        res.clear();
        parser.preg_match(" +([a-zA-Z]+) +([a-zA-Z]+) +([a-zA-Z]+) ([a-zA-Z]+) *", 
                          " const long  int var ;", 
                          res);
        CPPUNIT_ASSERT(res.size() == 4);
        CPPUNIT_ASSERT(res[0] == "const"); 
        CPPUNIT_ASSERT(res[1] == "long");
        CPPUNIT_ASSERT(res[2] == "int");
        CPPUNIT_ASSERT(res[3] == "var");

    }

    void testGetVarName() 
    {
        Parser parser;
        string name;
       
        CPPUNIT_ASSERT(parser.getVarName("int var;", name) == true);
        CPPUNIT_ASSERT(name == "var");

        CPPUNIT_ASSERT(parser.getVarName("const int lol   ;   ", name));
        CPPUNIT_ASSERT(name == "lol");

        CPPUNIT_ASSERT(parser.getVarName("volatile static float v,", name));
        CPPUNIT_ASSERT(name == "v");

        CPPUNIT_ASSERT(parser.getVarName("volatile float fff = 453.0;", name));
        CPPUNIT_ASSERT(name == "fff");

        CPPUNIT_ASSERT(parser.getVarName("const   unsigned int   zlo  = SOME_CONST ,", name));
        CPPUNIT_ASSERT(name == "zlo");
        
    }
};


CPPUNIT_TEST_SUITE_REGISTRATION(TestParser); 


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
