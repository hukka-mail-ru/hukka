#ifndef TestClientChess_H_
#define TestClientChess_H_

#include <cppunit/extensions/HelperMacros.h>
#include <QString>
#include "Client.h"
#include "Exception.h"
#include "Settings.h"

#define MY_USER_NAME            "test"
#define MY_USER_PASSWD          "test"


class TestClientChess: public CppUnit::TestFixture
{

        CPPUNIT_TEST_SUITE(TestClientChess);

        CPPUNIT_TEST(testTableJoin); // bug 00024

        CPPUNIT_TEST_SUITE_END();

        Client me;
        Client opponent;

        QNetworkProxy proxy;
        QList<Param> rightParams;

        TABLEID opponentTableID;

public:
        TestClientChess(): opponentTableID(0) {}

        void setUp()
        {
                SHOW_FUNCTION_NAME;

                me.setName("ME");
                opponent.setName("OPPONENT");

            //    qDebug() << "setUp()";
                proxy=QNetworkProxy(RIGHT_PROXY_TYPE, RIGHT_PROXY_HOSTNAME, RIGHT_PROXY_PORT);

                // opponent connects
            //    qDebug() << "1";
                CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, opponent.getClientStatus());
                CPPUNIT_ASSERT_NO_THROW(opponent.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
             //   qDebug() << "2";

                CPPUNIT_ASSERT_NO_THROW(opponent.login(OPPONENT_USER_NAME, OPPONENT_USER_PASSWD));
                CPPUNIT_ASSERT_EQUAL(CLI_AUTHORIZED, opponent.getClientStatus());
             //   qDebug() << "3";

                // opponent creates a game table
                TABLEID tableId = opponent.getMyGameTable(LOGIC_ID_CHESS);
                if(tableId) {
             //           CPPUNIT_ASSERT_NO_THROW(opponent.deleteGameTable(LOGIC_ID_CHESS, tableId));
                }

                rightParams.clear();
                Param rp1 = { PARAMETER_ID_TIME2STEP, RIGHT_TIME2STEP, 0, 0 };
                Param rp2 = { PARAMETER_ID_TIME2GAME, RIGHT_TIME2GAME, 0, 0 };
                Param rp3 = { PARAMETER_ID_MAXRATING, RIGHT_MAXRATING, 0, 0 };
                Param rp4 = { PARAMETER_ID_MINRATING, RIGHT_MINRATING, 0, 0 };
                rightParams << rp1 << rp2 << rp3 << rp4;

                CPPUNIT_ASSERT_NO_THROW(opponentTableID = opponent.createGameTable(LOGIC_ID_CHESS, rightParams));


                // I connect
                CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, me.getClientStatus());
                CPPUNIT_ASSERT_NO_THROW(me.connectToHost(proxy, RIGHT_SERVER_HOSTNAME, RIGHT_SERVER_PORT));
                CPPUNIT_ASSERT_NO_THROW(me.login(MY_USER_NAME, MY_USER_PASSWD));
                CPPUNIT_ASSERT_EQUAL(CLI_AUTHORIZED, me.getClientStatus());

        }

        void tearDown()
        {
                SHOW_FUNCTION_NAME;

                // opponent disconnects
                CPPUNIT_ASSERT_NO_THROW(opponent.disconnectFromHost());
                CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, opponent.getClientStatus());

                // I disconnect
                CPPUNIT_ASSERT_NO_THROW(me.disconnectFromHost());
                CPPUNIT_ASSERT_EQUAL(CLI_DISCONNECTED, me.getClientStatus());
        }

        void testTableJoin()
        {
                SHOW_FUNCTION_NAME;


                CPPUNIT_ASSERT_EQUAL(GAM_STOPPED, opponent.getGameStatus());

                CPPUNIT_ASSERT_NO_THROW(me.joinGameTable(opponentTableID));

                for(int i=0; i<WAIT_RESPONSE_TIMEOUT/1000; ++i)
                {
                    if(opponent.getGameStatus() == GAM_OPPONENT_JOINED)
                        break;

                    sleep(1);
                }

                CPPUNIT_ASSERT_EQUAL(GAM_OPPONENT_JOINED, opponent.getGameStatus());

                opponent.deleteGameTable(LOGIC_ID_CHESS, opponentTableID);

//                CPPUNIT_ASSERT_NO_THROW(opponent.agreeToStartGame(opponentTableID));

                //sleep(10);
/*
                me.mSocket.waitForReadyRead();
                CPPUNIT_ASSERT_EQUAL(me.getGameStatus(), GAM_STARTED);

                Field field;
                CPPUNIT_ASSERT_NO_THROW(me.getField(opponentTableID, field));
*/

               // CPPUNIT_ASSERT_EQUAL(field.whitePlayer.pieces.size(), CELLS_IN_FIELD/2);
               // CPPUNIT_ASSERT_EQUAL(field.blackPlayer.pieces.size(), CELLS_IN_FIELD/2);

               // CPPUNIT_ASSERT_EQUAL(field.activePlayer == field.whitePlayer || field.activePlayer == field.blackPlayer, true);

                //CPPUNIT_ASSERT_NO_THROW(opponent.deleteGameTable(LOGIC_ID_CHESS, opponentTableID));
                //CPPUNIT_ASSERT_THROW(me.joinGameTable(opponentTableID), Exception);

        }

};


CPPUNIT_TEST_SUITE_REGISTRATION(TestClientChess);

#endif /*TestClientChess_H_*/
