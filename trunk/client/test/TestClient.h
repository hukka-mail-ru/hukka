#ifndef TestAnimation_H_
#define TestAnimation_H_


#include <cppunit/extensions/HelperMacros.h>

#include "Client.h"

class TestClient: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestClient);
   CPPUNIT_TEST(testConnect);
   CPPUNIT_TEST(testDisconnect);
   CPPUNIT_TEST(testLogin);
   CPPUNIT_TEST(testIsOnline);
   CPPUNIT_TEST_SUITE_END();
         
public:         
    void setUp() {}
    void tearDown() {}
    
    void testConnect()
    {
//        SHOW_FUNCTION_NAME;
    }
    
    void testDisconnect() 
    {
 //       SHOW_FUNCTION_NAME;
    }
    

    void testLogin()
    {
//        SHOW_FUNCTION_NAME;
    }
    
    void testIsOnline() 
    {
 //       SHOW_FUNCTION_NAME;
    }





};


CPPUNIT_TEST_SUITE_REGISTRATION(TestClient); 
   
#endif /*TestAnimation_H_*/
