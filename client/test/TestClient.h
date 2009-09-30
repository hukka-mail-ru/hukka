#ifndef TestClient_H_
#define TestClient_H_


#include <cppunit/extensions/HelperMacros.h>

#include "Client.h"

#define RIGHT_SERVER_HOSTNAME   "10.233.103.241" //"wapserver3.wapportal.ru"
#define RIGHT_SERVER_PORT       3333 //1234

#define RIGHT_PROXY_HOSTNAME    "proxy.t-systems.ru"
#define RIGHT_PROXY_PORT        3128
#define RIGHT_PROXY_TYPE        QNetworkProxy::HttpProxy // supports only outgoing TCP connections

#define RIGTH_USER_NAME         "test"
#define RIGTH_USER_PASSWD       "test"

class TestClient: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestClient);
   CPPUNIT_TEST(testConnectOK);
 //  CPPUNIT_TEST(testConnectWrongProxy);
   CPPUNIT_TEST(testConnectWrongAddress);
   CPPUNIT_TEST(testConnectWrongPort);
/*
   CPPUNIT_TEST(testLoginOK);
   CPPUNIT_TEST(testLoginWrongUser);
   CPPUNIT_TEST(testLoginWrongPasswd);
   CPPUNIT_TEST(testLoginAlreadyOnLine);
*/
   CPPUNIT_TEST_SUITE_END();

    Client client;
    QNetworkProxy proxy;

         
public:   
    ~TestClient() {}
              
    void setUp() 
    {
        proxy=QNetworkProxy(RIGHT_PROXY_TYPE, RIGHT_PROXY_HOSTNAME, RIGHT_PROXY_PORT);
    }

    void tearDown() {}

    
    void testConnectOK()
    {
//        SHOW_FUNCTION_NAME;
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        client.disconnectFromHost();
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }


    void testConnectWrongProxy()
    {
//        SHOW_FUNCTION_NAME;
        proxy.setHostName("wrong");

        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }

    void testConnectWrongAddress()
    {
//        SHOW_FUNCTION_NAME;
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        client.connectToHost(proxy, "wrong", RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }

    void testConnectWrongPort()
    {
//        SHOW_FUNCTION_NAME;
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, 0);
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }



    void testLoginOK()
    {
//        SHOW_FUNCTION_NAME;
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        CPPUNIT_ASSERT_EQUAL(LOG_OK, client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD));

        CPPUNIT_ASSERT_EQUAL(LOG_WRONG_USER, client.logout("wrong"));
        CPPUNIT_ASSERT_EQUAL(LOG_OK, client.logout(RIGTH_USER_NAME));

        client.disconnectFromHost();
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }

   void testLoginWrongUser()
   {
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        LogStatus st = client.login("wrong", RIGTH_USER_PASSWD);
        CPPUNIT_ASSERT_EQUAL(LOG_WRONG_USER, st);

        client.disconnectFromHost();
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
   }

   void testLoginWrongPasswd()
   {
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        LogStatus st = client.login(RIGTH_USER_NAME, "wrong");
        CPPUNIT_ASSERT_EQUAL(LOG_WRONG_PASSWD, st);

        client.disconnectFromHost();
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
   }

   void testLoginAlreadyOnLine()
   {
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT);
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        CPPUNIT_ASSERT_EQUAL(LOG_OK, client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD));

        CPPUNIT_ASSERT_EQUAL(LOG_USER_ONLINE, client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD));

        client.disconnectFromHost();
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
   }



};


CPPUNIT_TEST_SUITE_REGISTRATION(TestClient); 
   
#endif /*TestClient_H_*/
