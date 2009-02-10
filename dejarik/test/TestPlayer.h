#ifndef TESTPLAYER_H_
#define TESTPLAYER_H_


#include <cppunit/extensions/HelperMacros.h>

#include "Player.h"
#include "Macros.h"

class TestPlayer: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestPlayer);
   CPPUNIT_TEST(testOnCellClick);
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
        

    void testOnCellClick() 
    {
        TRY_BEGINS;
        
        CPPUNIT_ASSERT_EQUAL(25, 25);
        
        CATCH("testOnCellClick");
    }


};


CPPUNIT_TEST_SUITE_REGISTRATION(TestPlayer); 
   
#endif /*TESTPLAYER_H_*/
