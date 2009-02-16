#ifndef TestGameH_
#define TestGameH_


#include <cppunit/extensions/HelperMacros.h>

#include "Player.h"
#include "Macros.h"

class TestGame: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestGame);
   CPPUNIT_TEST(test1);
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
        

    void test1() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        CPPUNIT_ASSERT_EQUAL(25, 25);
        
        TRY_CATCH;
    }


};


CPPUNIT_TEST_SUITE_REGISTRATION(TestGame); 
   
#endif /*TestGameH_*/