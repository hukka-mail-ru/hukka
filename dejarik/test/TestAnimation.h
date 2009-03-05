#ifndef TestAnimation_H_
#define TestAnimation_H_


#include <cppunit/extensions/HelperMacros.h>

#include "Animation.h"
#include "Macros.h"

class TestAnimation: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestAnimation);
   CPPUNIT_TEST(testGetNormalAngle);
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
        

    void testGetNormalAngle() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        Animation anim;
        
        CPPUNIT_ASSERT_EQUAL((float)0, 
                anim.getNormalAngle(CIRCLE_CENTER_X , CIRCLE_CENTER_Y + 10));

        CPPUNIT_ASSERT_EQUAL((float)180, 
                anim.getNormalAngle(CIRCLE_CENTER_X , CIRCLE_CENTER_Y - 10));
        
        CPPUNIT_ASSERT_EQUAL((float)90, 
                anim.getNormalAngle(CIRCLE_CENTER_X + 10 , CIRCLE_CENTER_Y));

        float ang = anim.getNormalAngle(CIRCLE_CENTER_X - 10 , CIRCLE_CENTER_Y);
        CPPUNIT_ASSERT_EQUAL((float)270, ang);
        
        
        CPPUNIT_ASSERT_EQUAL((float)45, 
                anim.getNormalAngle(CIRCLE_CENTER_X + 10 , CIRCLE_CENTER_Y + 10));
        
        CPPUNIT_ASSERT_EQUAL((float)135, 
                anim.getNormalAngle(CIRCLE_CENTER_X + 10 , CIRCLE_CENTER_Y - 10));

        CPPUNIT_ASSERT_EQUAL((float)225, 
                anim.getNormalAngle(CIRCLE_CENTER_X - 10 , CIRCLE_CENTER_Y - 10));

        CPPUNIT_ASSERT_EQUAL((float)315, 
                anim.getNormalAngle(CIRCLE_CENTER_X - 10 , CIRCLE_CENTER_Y + 10));

        TRY_CATCH("testGetNormalAngle");
    }


};


CPPUNIT_TEST_SUITE_REGISTRATION(TestAnimation); 
   
#endif /*TestAnimation_H_*/
