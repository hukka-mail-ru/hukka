#ifndef TestAnimation_H_
#define TestAnimation_H_


#include <cppunit/extensions/HelperMacros.h>

#include "Animation.h"
#include "Macros.h"

class TestAnimation: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestAnimation);
   CPPUNIT_TEST(testGetNormalAngle);
   CPPUNIT_TEST(testGetTargetAngle);
   CPPUNIT_TEST(testGetSmallestAngle);
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
    
    void testGetSmallestAngle() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Animation anim;
        
        CPPUNIT_ASSERT_EQUAL((float)-40, anim.getSmallestAngle(70 , 30));
        CPPUNIT_ASSERT_EQUAL((float)90, anim.getSmallestAngle(0 , 90));
        CPPUNIT_ASSERT_EQUAL((float)0, anim.getSmallestAngle(360 , 0));
        CPPUNIT_ASSERT_EQUAL((float)-10, anim.getSmallestAngle(150 , 140));
        CPPUNIT_ASSERT_EQUAL((float)-170, anim.getSmallestAngle(90 , 280));
        CPPUNIT_ASSERT_EQUAL((float)-170, anim.getSmallestAngle(260 , 90));
        CPPUNIT_ASSERT_EQUAL((float)170, anim.getSmallestAngle(190 , 0));
        CPPUNIT_ASSERT_EQUAL((float)-160, anim.getSmallestAngle(190 , 30));
        CPPUNIT_ASSERT_EQUAL((float)0, anim.getSmallestAngle(0 , 0));
        CPPUNIT_ASSERT_EQUAL((float)-90, anim.getSmallestAngle(270 , 180));
        
        TRY_CATCH;
    }
    
    void testGetTargetAngle() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        
        Animation anim;
        Board board;
        
        anim.mMoveSteps.clear();
        anim.mMoveSteps.push_back(board.getCell(2,0));
        anim.mMoveSteps.push_back(board.getCell(2,11));        
        CPPUNIT_ASSERT_EQUAL((float)(360-15), anim.getTargetAngle(0));
        
        anim.mMoveSteps.clear();
        anim.mMoveSteps.push_back(board.getCell(2,11));
        anim.mMoveSteps.push_back(board.getCell(2,0));        
        CPPUNIT_ASSERT_EQUAL((float)(180+15), anim.getTargetAngle(0));
        
        anim.mMoveSteps.clear();
        anim.mMoveSteps.push_back(board.getCell(0,0));
        anim.mMoveSteps.push_back(board.getCell(2,2));        
        CPPUNIT_ASSERT_EQUAL((float)(180+15), anim.getTargetAngle(0));
        
        
        TRY_CATCH;
    }
        
        

    void testGetNormalAngle() 
    {
        TRY_BEGINS;
        SHOW_FUNCTION_NAME;
        Animation anim;
        
        CPPUNIT_ASSERT_EQUAL((float)0, 
                anim.getNormalAngle(CIRCLE_CENTER_X , CIRCLE_CENTER_Y ));
        
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

        TRY_CATCH;
    }


};


CPPUNIT_TEST_SUITE_REGISTRATION(TestAnimation); 
   
#endif /*TestAnimation_H_*/
