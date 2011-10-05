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
   //     CPPUNIT_TEST(testTableCreateWithoutAuthorization);
        CPPUNIT_TEST(testTableGetMy);
   //     CPPUNIT_TEST(testTableCreateWrongParams);
   //     CPPUNIT_TEST(testTableGetRandom);
   //     CPPUNIT_TEST(testTableGetParam);

     //   CPPUNIT_TEST(testTableSetParam); // not implemented on server

  //     CPPUNIT_TEST(testTableFind1);
  //      CPPUNIT_TEST(testTableFindNothing);

        CPPUNIT_TEST_SUITE_END();

        Client client;
        QNetworkProxy proxy;
        QList<Param> rightParams;


public:
        ~TestClientTable() {}

        void setUp()
        {
                proxy=QNetworkProxy(RIGHT_PROXY_TYPE, RIGHT_PROXY_HOSTNAME, RIGHT_PROXY_PORT);

                // connect
                CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, client.getClientStatus());
                CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
                CPPUNIT_ASSERT_NO_THROW(client.login(RIGTH_USER_NAME, RIGTH_USER_PASSWD));
                CPPUNIT_ASSERT_EQUAL(CLI_AUTHORIZED, client.getClientStatus());

                // delete my table if exists
                TABLEID id = client.getMyGameTable(LOGIC_ID_CHESS);
                if(id) {
                        CPPUNIT_ASSERT_NO_THROW(client.deleteGameTable(LOGIC_ID_CHESS, id));
                }

                // setup parameters;
                rightParams.clear();
                Param p1 = { PARAMETER_ID_TIME2STEP, RIGHT_TIME2STEP, 0, 0 };
                Param p2 = { PARAMETER_ID_TIME2GAME, RIGHT_TIME2GAME, 0, 0 };
                Param p3 = { PARAMETER_ID_MAXRATING, RIGHT_MAXRATING, 0, 0 };
               Param p4 = { PARAMETER_ID_MINRATING, RIGHT_MINRATING, 0, 0 };
                rightParams << p1 << p2 << p3 << p4;
        }

        void tearDown()
        {
                CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
                CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, client.getClientStatus());
        }

        void testTableCreateOK()
        {
                SHOW_FUNCTION_NAME;

                TABLEID id = 0;
                CPPUNIT_ASSERT_NO_THROW(id = client.createGameTable(LOGIC_ID_CHESS, rightParams));
                CPPUNIT_ASSERT(id > 0);
                CPPUNIT_ASSERT(id == client.getMyGameTable(LOGIC_ID_CHESS));
        }

        void testTableCreateWithoutAuthorization()
        {
                SHOW_FUNCTION_NAME;
                CPPUNIT_ASSERT_NO_THROW(client.disconnectFromHost());
                CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, client.getClientStatus());
                CPPUNIT_ASSERT_NO_THROW(client.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
                CPPUNIT_ASSERT_EQUAL(CLI_CONNECTED, client.getClientStatus());

                CPPUNIT_ASSERT_THROW(client.createGameTable(LOGIC_ID_CHESS, rightParams), Exception);
        }

        void testTableGetMy()
        {
                SHOW_FUNCTION_NAME;
                TABLEID id = 0;
                CPPUNIT_ASSERT_NO_THROW(id = client.getMyGameTable(LOGIC_ID_CHESS));
           //     qDebug() << "My table: " << QString::number(id);
        }

        void testTableCreateWrongParams()
        {
                SHOW_FUNCTION_NAME;

                TABLEID id = 0;
                Param p1 = { PARAMETER_ID_TIME2STEP, PARAMETER_MAX_TIME2STEP + 1, 0, 0 };
                QList<Param> wrongParams;
                wrongParams << p1;
                CPPUNIT_ASSERT_THROW(id = client.createGameTable(LOGIC_ID_CHESS, wrongParams), Exception);
                CPPUNIT_ASSERT(id == 0);

                Param p2 = { PARAMETER_ID_TIME2STEP, PARAMETER_MAX_TIME2STEP + 1, 0, 0 };
                wrongParams.clear();
                wrongParams << p2;
                CPPUNIT_ASSERT_THROW(id = client.createGameTable(LOGIC_ID_CHESS, wrongParams), Exception);
                CPPUNIT_ASSERT(id == 0);

                // TODO add tests for minRating and maxRating
        }

        void testTableGetParam() // TODO See bug 0000022
        {
                SHOW_FUNCTION_NAME;

                TABLEID id = 0;
                CPPUNIT_ASSERT_NO_THROW(id = client.createGameTable(LOGIC_ID_CHESS, rightParams));
                CPPUNIT_ASSERT(id > 0);

                Param param;
                CPPUNIT_ASSERT_NO_THROW(param = client.getGameTableParam(LOGIC_ID_CHESS, id, PARAMETER_ID_TIME2STEP));
                CPPUNIT_ASSERT(param.value == RIGHT_TIME2STEP);

                CPPUNIT_ASSERT_NO_THROW(param = client.getGameTableParam(LOGIC_ID_CHESS, id, PARAMETER_ID_TIME2GAME));
                CPPUNIT_ASSERT(param.value == RIGHT_TIME2GAME);

                CPPUNIT_ASSERT_NO_THROW(param = client.getGameTableParam(LOGIC_ID_CHESS, id, PARAMETER_ID_MINRATING));
                CPPUNIT_ASSERT(param.value == RIGHT_MINRATING);

                CPPUNIT_ASSERT_NO_THROW(param = client.getGameTableParam(LOGIC_ID_CHESS, id, PARAMETER_ID_MAXRATING));
                CPPUNIT_ASSERT(param.value == RIGHT_MAXRATING);
        }
/*
        void testTableSetParam()
        {
                SHOW_FUNCTION_NAME;

                TABLEID id = 0;
                CPPUNIT_ASSERT_NO_THROW(id = client.createGameTable(LOGIC_ID_CHESS, rightParams));
                CPPUNIT_ASSERT(id > 0);

                Param newparam;
                newparam.id = PARAMETER_ID_TIME2STEP;
                newparam.value = RIGHT_TIME2STEP / 2;
                CPPUNIT_ASSERT_NO_THROW(client.setGameTableParam(LOGIC_ID_CHESS, id, newparam));

                Param param;
                CPPUNIT_ASSERT_NO_THROW(param = client.getGameTableParam(LOGIC_ID_CHESS, id, newparam.id));
                CPPUNIT_ASSERT(param.value == newparam.value);
        }
*/
        // a helper
        void opponentCreatesTable()
        {
                // connect an opponent
                Client opponent;
                CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, opponent.getClientStatus());
                CPPUNIT_ASSERT_NO_THROW(opponent.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));

                CPPUNIT_ASSERT_NO_THROW(opponent.login(OPPONENT_USER_NAME, OPPONENT_USER_PASSWD));
                CPPUNIT_ASSERT_EQUAL(CLI_AUTHORIZED, opponent.getClientStatus());

                // opponent creates a game table
                TABLEID tableId = opponent.getMyGameTable(LOGIC_ID_CHESS);
                if(tableId) {
                        CPPUNIT_ASSERT_NO_THROW(opponent.deleteGameTable(LOGIC_ID_CHESS, tableId));
                }

                rightParams.clear();
                Param rp1 = { PARAMETER_ID_TIME2STEP, RIGHT_TIME2STEP, 0, 0 };
                Param rp2 = { PARAMETER_ID_TIME2GAME, RIGHT_TIME2GAME, 0, 0 };
                Param rp3 = { PARAMETER_ID_MAXRATING, RIGHT_MAXRATING, 0, 0 };
                Param rp4 = { PARAMETER_ID_MINRATING, RIGHT_MINRATING, 0, 0 };
                rightParams << rp1 << rp2 << rp3 << rp4;

                CPPUNIT_ASSERT_NO_THROW(opponent.createGameTable(LOGIC_ID_CHESS, rightParams));
        }

        void testTableGetRandom()
        {
                SHOW_FUNCTION_NAME;

                opponentCreatesTable();

                // I try to connect to the opponent's table
                QList<Param> condition;
                Param p1 = { PARAMETER_ID_TIME2STEP, RIGHT_TIME2STEP, OPERATOR_EQUAL, OPERATOR_LAST };
                //   Param p2 = { PARAMETER_ID_TIME2GAME, 0,                       OPERATOR_MORE, OPERATOR_AND };
                //    Param p3 = { PARAMETER_ID_MINRATING, 0,                       OPERATOR_EQUAL, OPERATOR_AND };
                //    Param p4 = { PARAMETER_ID_MAXRATING, 0,                       OPERATOR_EQUAL, OPERATOR_LAST };

                condition << p1;// << p2 << p3 << p4;



                unsigned id = 0;
                CPPUNIT_ASSERT_NO_THROW(id = client.getRandomGameTable(LOGIC_ID_CHESS, condition));
                CPPUNIT_ASSERT(id > 0);

                // TODO some more tests: OPERATOR_OR
        }

        void testTableFind1()
        {
                SHOW_FUNCTION_NAME;
                opponentCreatesTable();

                // I try to connect to the opponent's table
                QList<Param> condition;
                Param p1 = { PARAMETER_ID_TIME2STEP, RIGHT_TIME2STEP, OPERATOR_EQUAL, OPERATOR_LAST };
                condition << p1;

                TABLEID id = 0;
                unsigned maxCount = 10;
                QList<unsigned> ids;
                CPPUNIT_ASSERT_NO_THROW(client.findGameTables(LOGIC_ID_CHESS, maxCount, condition, ids));
                CPPUNIT_ASSERT(ids.size() > 0);
                CPPUNIT_ASSERT(ids.size() <= maxCount);

        }


        void testTableFindNothing()
        {
                SHOW_FUNCTION_NAME;

                CPPUNIT_ASSERT_NO_THROW(client.createGameTable(LOGIC_ID_CHESS, rightParams));

                QList<Param> condition;
                Param c1 = { PARAMETER_ID_TIME2STEP, RIGHT_TIME2STEP + 1, OPERATOR_EQUAL, OPERATOR_OR };
                Param c2 = { PARAMETER_ID_TIME2GAME, RIGHT_TIME2GAME + 1, OPERATOR_EQUAL, OPERATOR_OR };
                Param c3 = { PARAMETER_ID_MINRATING, RIGHT_MINRATING + 1, OPERATOR_EQUAL, OPERATOR_OR };
                Param c4 = { PARAMETER_ID_MAXRATING, RIGHT_MAXRATING + 1, OPERATOR_EQUAL, OPERATOR_LAST };

                condition << c1 << c2 << c3 << c4;

                TABLEID id = 0;
                unsigned maxCount = 10;
                QList<unsigned> ids;
                CPPUNIT_ASSERT_NO_THROW(client.findGameTables(LOGIC_ID_CHESS, maxCount, condition, ids));
                CPPUNIT_ASSERT(ids.size() == 0);
        }

};


CPPUNIT_TEST_SUITE_REGISTRATION(TestClientTable);

#endif /*TestClientTable_H_*/
