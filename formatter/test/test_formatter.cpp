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
   CPPUNIT_TEST(testPregMatch);
   CPPUNIT_TEST(testParseVar);
   CPPUNIT_TEST_SUITE_END();
         
   
public:         
    void setUp() {}
    void tearDown() {}
        

    void testPregMatch() 
    {
        Parser parser;
        string str;
        parser.preg_match(" *([a-zA-Z]+) *;", "   const   int var ;", str);
        CPPUNIT_ASSERT(str == "var");

        vector<string> res;
        parser.preg_match(" +([a-zA-Z]+) +([a-zA-Z]+) +([a-zA-Z]+) ([a-zA-Z]+) *", 
                          " const long  int var ;", 
                          res);
        CPPUNIT_ASSERT(res.size() == 4);
        CPPUNIT_ASSERT(res[0] == "const"); 
        CPPUNIT_ASSERT(res[1] == "long");
        CPPUNIT_ASSERT(res[2] == "int");
        CPPUNIT_ASSERT(res[3] == "var");

    }

    void testParseVar() 
    {
        Parser parser;
        Variable var;
       
        CPPUNIT_ASSERT(parser.parseVar("int var;", var) == true);
        CPPUNIT_ASSERT(var.name == "var");
        CPPUNIT_ASSERT(var.type == "int");

        CPPUNIT_ASSERT(parser.parseVar("const int lol   ;   ", var));
        CPPUNIT_ASSERT(var.name == "lol");
        CPPUNIT_ASSERT(var.type == "const int");

        CPPUNIT_ASSERT(parser.parseVar("volatile static float v,", var));
        CPPUNIT_ASSERT(var.name == "v");
        CPPUNIT_ASSERT(var.type == "volatile static float");

        CPPUNIT_ASSERT(parser.parseVar(" volatile float fff = 453.0;", var));
        CPPUNIT_ASSERT(var.name == "fff");
        CPPUNIT_ASSERT(var.type == "volatile float");

        CPPUNIT_ASSERT(parser.parseVar("  const   unsigned int   zlo  = SOME_CONST ,", var));
        CPPUNIT_ASSERT(var.name == "zlo");
        CPPUNIT_ASSERT(var.type == "const   unsigned int");
        
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
