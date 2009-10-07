#ifndef TestClientLogin_H_
#define TestClientLogin_H_

#include <cppunit/extensions/HelperMacros.h>
#include "Client.h"
#include "Exception.h"
#include "Settings.h"


class TestClientLogin: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestClientLogin);

   CPPUNIT_TEST(testGetCRC);
   CPPUNIT_TEST(testConnectOK);
   CPPUNIT_TEST(testConnectWrongAddress);
   CPPUNIT_TEST(testConnectWrongPort);

   CPPUNIT_TEST(testLoginOK);
   CPPUNIT_TEST(testLoginWrongUser);
   CPPUNIT_TEST(testLoginWrongPasswd);
   CPPUNIT_TEST(testLoginAlreadyOnLine);
   CPPUNIT_TEST(testLoginAlreadyDisconnected);

   CPPUNIT_TEST_SUITE_END();

    Client client;
    QNetworkProxy proxy;

         
public:   
    ~TestClientLogin() {}
              
    void setUp() 
    {
        proxy=QNetworkProxy(RIGHT_PROXY_TYPE, RIGHT_PROXY_HOSTNAME, RIGHT_PROXY_PORT);
    }

    void tearDown() {}

    void testGetCRC()
    {
        SHOW_FUNCTION_NAME;

        QByteArray arr;
        arr.push_back(2);
        arr.push_back(1);
        arr.push_back((char)0);
        arr.push_back(127);
        arr.push_back(2);

        CPPUNIT_ASSERT_EQUAL(126, (int)getCRC(arr));
    }
    
    void testConnectOK()
    { 
        SHOW_FUNCTION_NAME;
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }


    void testConnectWrongProxy()
    {
        SHOW_FUNCTION_NAME;
        proxy.setHostName("wrong_server_name");

        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        CPPUNIT_ASSERT_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT), Exception);
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }

    void testConnectWrongAddress()
    {
        SHOW_FUNCTION_NAME;
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        CPPUNIT_ASSERT_THROW(client.connectToHost(proxy, "wrong_server_name", RIGHT_SERVER_PORT), Exception);
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }

    void testConnectWrongPort()
    {
        SHOW_FUNCTION_NAME;
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        CPPUNIT_ASSERT_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, 0), Exception);
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }



    void testLoginOK()
    {
        SHOW_FUNCTION_NAME;
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        CPPUNIT_ASSERT_NO_THROW(client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD));

        CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }

   void testLoginWrongUser()
   {
        SHOW_FUNCTION_NAME;

        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        CPPUNIT_ASSERT_THROW(client.login("wrong_server_name", RIGTH_USER_PASSWD), Exception);

        CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
   }

   void testLoginWrongPasswd()
   {
        SHOW_FUNCTION_NAME;

        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        CPPUNIT_ASSERT_THROW(client.login(RIGTH_USER_NAME, "wrong_server_name"), Exception);

        CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
   }

   void testLoginAlreadyOnLine()
   {
        SHOW_FUNCTION_NAME;

        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        CPPUNIT_ASSERT_NO_THROW(client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD));
        CPPUNIT_ASSERT_THROW(client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD), Exception);

        CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
   }


   void testLoginAlreadyDisconnected()
   {
        SHOW_FUNCTION_NAME;

        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());

        CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());

        CPPUNIT_ASSERT_THROW(client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD), Exception);
   }
};


CPPUNIT_TEST_SUITE_REGISTRATION(TestClientLogin); 
   
#endif /*TestClientLogin_H_*/
