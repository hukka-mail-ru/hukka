#include <iostream>
#include <string>
#include <cstdlib>

#include <cppunit/TestAssert.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/XmlOutputter.h>
#include <cppunit/portability/Stream.h>


//#include "TestBoard.h"
//#include "TestPlayer.h"
#include "TestGame.h"
#include "TestAnimation.h"

using namespace std;



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
