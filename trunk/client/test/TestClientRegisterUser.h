#ifndef TestClientRegisterUser_H_
#define TestClientRegisterUser_H_

#include <cppunit/extensions/HelperMacros.h>
#include "Client.h"
#include "Exception.h"
#include "Settings.h"


/* ====================================================================================================
 32-letter unique ID
  __  ___  ____     __  _  _  __  ___  
 / _)(  _)(_  _)   / _)( )( )(  )(   \ 
( (/\ ) _)  )(    ( (/\ )()(  )(  ) ) )
 \__/(___) (__)    \__/ \__/ (__)(___/ 
==================================================================================================== */
QString getGUID()
{
        QString str;
        srand((unsigned)time(0));  // TODO replace 'time' with QT func.
        for(int i=0; i<32; i++) {        
                int rnd = 0;
                for(;;) {
                        rnd = (rand()%(int)'F') + (int)'0';
                        if((rnd >= 48 && rnd <=57) || (rnd >= 65 && rnd <=70))
                                break;
                 }
                str += QChar(rnd);
        }

        return str;
}
/*====================================================================================================
 ____  ___  ___  ____    ___   ___   __  __  ___  ____  ___  ___  
(_  _)(  _)/ __)(_  _)  (  ,) (  _) / _)(  )/ __)(_  _)(  _)(  ,) 
  )(   ) _)\__ \  )(     )  \  ) _)( (/\ )( \__ \  )(   ) _) )  \ 
 (__) (___)(___/ (__)   (_)\_)(___) \__/(__)(___/ (__) (___)(_)\_) 
==================================================================================================== */

class TestClientRegisterUser: public CppUnit::TestFixture 
{
   CPPUNIT_TEST_SUITE(TestClientRegisterUser);

   CPPUNIT_TEST(testRegisterOK);
   CPPUNIT_TEST(testRegisterWrongLogin);
   CPPUNIT_TEST(testRegisterWrongPassword);
   CPPUNIT_TEST(testRegisterLoginExists);

   CPPUNIT_TEST_SUITE_END();

    Client client;
    QNetworkProxy proxy;

         
public:   
             
    void setUp() 
    {
        proxy=QNetworkProxy(RIGHT_PROXY_TYPE, RIGHT_PROXY_HOSTNAME, RIGHT_PROXY_PORT);

        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
        CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
        CPPUNIT_ASSERT_EQUAL(CLI_ONLINE, client.status());
    }

    void tearDown() 
    {
        CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
        CPPUNIT_ASSERT_EQUAL(CLI_OFFLINE, client.status());
    }

    void testRegisterOK() 
    {
        SHOW_FUNCTION_NAME;
        
        QString unique = "test-" + getGUID();
        qDebug() << "Generated user name:" << unique;
        CPPUNIT_ASSERT_NO_THROW(client.registerUser(unique, unique));

        // TODO test connect
    }

    void testRegisterWrongLogin() 
    {
        SHOW_FUNCTION_NAME;

        CPPUNIT_ASSERT_THROW(client.registerUser("--f&sd", RIGTH_USER_PASSWD), Exception); // wrong symbols
        CPPUNIT_ASSERT_THROW(client.registerUser("s", RIGTH_USER_PASSWD), Exception); // too short
        CPPUNIT_ASSERT_THROW(client.registerUser("sfhvjkdfhvjkdfhvkhdfskvhkjdfshkhdfvkjsdhfvjkdfhkvhdkfshvkjsdfhvkd", RIGTH_USER_PASSWD), Exception); 
        
    }

    void testRegisterWrongPassword() 
    {
        SHOW_FUNCTION_NAME;

        CPPUNIT_ASSERT_THROW(client.registerUser(RIGTH_USER_NAME, "--f&sd"), Exception); // wrong symbols
        CPPUNIT_ASSERT_THROW(client.registerUser(RIGTH_USER_NAME, "s"), Exception); // too short
        CPPUNIT_ASSERT_THROW(client.registerUser(RIGTH_USER_NAME, "sfhvjkdfhvjkdfhvkhdfskvhkjdfshkhdfvkjsdhfvjkdfhkvhdkfshvkjsdfhvkd"), Exception); 
    }

    void testRegisterLoginExists() 
    {
        SHOW_FUNCTION_NAME;

        CPPUNIT_ASSERT_THROW(client.registerUser(RIGTH_USER_NAME, RIGTH_USER_PASSWD), Exception); // already exists
    }

};


CPPUNIT_TEST_SUITE_REGISTRATION(TestClientRegisterUser); 
   
#endif /*TestClientRegisterUser_H_*/
