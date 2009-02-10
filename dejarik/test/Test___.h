#ifndef Test____H_
#define Test____H_


#include <cppunit/extensions/HelperMacros.h>

#include "Player.h"
#include "Macros.h"

class Test___: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(Test___);
   CPPUNIT_TEST(test1);
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
        

    void test1() 
    {
        TRY_BEGINS;
        
        CPPUNIT_ASSERT_EQUAL(25, 25);
        
        CATCH("test1");
    }


};


CPPUNIT_TEST_SUITE_REGISTRATION(Test___); 
   
#endif /*Test____H_*/
