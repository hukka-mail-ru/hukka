#ifndef TestClientTable_H_
#define TestClientTable_H_

#include <cppunit/extensions/HelperMacros.h>
#include "Client.h"
#include "Exception.h"
#include "Settings.h"


class TestClientTable: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestClientTable);

   CPPUNIT_TEST(testTableCreateOK);
   CPPUNIT_TEST(testTableCreateWithoutAuthorization);

   CPPUNIT_TEST_SUITE_END();

    Client client;
    QNetworkProxy proxy;

         
public:   
    ~TestClientTable() {}
              
    void setUp() 
    {
        proxy=QNetworkProxy(RIGHT_PROXY_TYPE, RIGHT_PROXY_HOSTNAME, RIGHT_PROXY_PORT);

        CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, client.status());
        CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
        CPPUNIT_ASSERT_NO_THROW(client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD));
        CPPUNIT_ASSERT_EQUAL(CLI_AUTHORIZED, client.status());
    }

    void tearDown() 
    {
        CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
        CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, client.status());
    }

    void testTableCreateOK()
    {
        SHOW_FUNCTION_NAME;
        quint32 id = 0;
        CPPUNIT_ASSERT_NO_THROW(id = client.createGameTable(LOGIC_ID_CHESS));
        CPPUNIT_ASSERT(id > 0);
    }

    void testTableCreateWithoutAuthorization()
    {
        CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
        CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, client.status());
        CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
        CPPUNIT_ASSERT_EQUAL(CLI_CONNECTED, client.status());

        CPPUNIT_ASSERT_THROW(client.createGameTable(LOGIC_ID_CHESS), Exception);
    }

};


CPPUNIT_TEST_SUITE_REGISTRATION(TestClientTable); 
   
#endif /*TestClientTable_H_*/
