#ifndef TestAnimation_H_
#define TestAnimation_H_


#include <cppunit/extensions/HelperMacros.h>

#include "Client.h"

#define RIGHT_SERVER_ADDRESS    unknown
#define RIGHT_SERVER_PORT       0

#define RIGHT_PROXY_ADDRESS     "proxy.t-systems.ru"
#define RIGHT_PROXY_PORT        3128
#define RIGHT_PROXY_TYPE        QNetworkProxy::HttpProxy // supports only outgoing TCP connections

#define RIGTH_USER_NAME         "unknown"
#define RIGTH_USER_PASSWD       "unknown"

class TestClient: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestClient);
   CPPUNIT_TEST(testConnectOK);
   CPPUNIT_TEST(testConnectWrongProxy);
   CPPUNIT_TEST(testConnectWrongAddress);
   CPPUNIT_TEST(testConnectWrongPort);

   CPPUNIT_TEST(testLoginOK);
   CPPUNIT_TEST(testLoginWrongUser);
   CPPUNIT_TEST(testLoginWrongPasswd);
   CPPUNIT_TEST(testLoginAlreadyOnLine);

   CPPUNIT_TEST_SUITE_END();

    Client client;
    QNetworkProxy proxy;
         
public:         
    void setUp() 
    {   
        proxy.setHostName(RIGHT_PROXY_ADDRESS);
        proxy.setPort(RIGHT_PROXY_PORT);
        proxy.setType(RIGHT_PROXY_TYPE);
    }

    void tearDown() {}

    
    void testConnectOK()
    {
//        SHOW_FUNCTION_NAME;
        client.connect(proxy, RIGHT_SERVER_ADDRESS, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        client.disconnect();
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }


    void testConnectWrongProxy()
    {
//        SHOW_FUNCTION_NAME;
        proxy.setHostName("wrong");

        client.connect(proxy, RIGHT_SERVER_ADDRESS, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }

    void testConnectWrongAddress()
    {
//        SHOW_FUNCTION_NAME;
        client.connect(proxy, "wrong", RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }

    void testConnectWrongPort()
    {
//        SHOW_FUNCTION_NAME;
        client.connect(proxy, RIGHT_SERVER_ADDRESS, 0);
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }



    void testLoginOK()
    {
//        SHOW_FUNCTION_NAME;
        client.connect(proxy, RIGHT_SERVER_ADDRESS, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        LoginStatus st = client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD);
        CPPUNIT_ASSERT_EQUAL(LIN_OK, st);

        LoginStatus st = client.logout(RIGTH_USER_NAME);
        CPPUNIT_ASSERT_EQUAL(LOU_OK, st);

        client.disconnect();
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }

   void testLoginWrongUser()
   {
        client.connect(proxy, RIGHT_SERVER_ADDRESS, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        LoginStatus st = client.login("wrong", RIGTH_USER_PASSWD);
        CPPUNIT_ASSERT_EQUAL(LIN_WRONG_LOGIN, st);

        client.disconnect();
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
   }

   void testLoginWrongPasswd()
   {
        client.connect(proxy, RIGHT_SERVER_ADDRESS, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        LoginStatus st = client.login(RIGTH_USER_NAME, "wrong");
        CPPUNIT_ASSERT_EQUAL(LIN_WRONG_PASSWD, st);

        client.disconnect();
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
   }

   void testLoginAlreadyOnLine()
   {
        client.connect(proxy, RIGHT_SERVER_ADDRESS, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        LoginStatus st = client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD);
        CPPUNIT_ASSERT_EQUAL(LIN_OK, st);

        LoginStatus st = client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD);
        CPPUNIT_ASSERT_EQUAL(LIN_USER_ONLINE, st);

        client.disconnect();
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
   }



};


CPPUNIT_TEST_SUITE_REGISTRATION(TestClient); 
   
#endif /*TestAnimation_H_*/
