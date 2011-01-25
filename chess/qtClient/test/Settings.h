#ifndef Settings_H_
#define Settings_H_

#define RIGHT_SERVER_HOSTNAME   DEFAULT_SERVER_HOSTNAME
#define RIGHT_SERVER_PORT       DEFAULT_SERVER_PORT

#define RIGHT_PROXY_HOSTNAME    "" // "proxy.t-systems.ru"
#define RIGHT_PROXY_PORT        0  // 3128
#define RIGHT_PROXY_TYPE        QNetworkProxy::HttpProxy // supports only outgoing TCP connections

#define RIGTH_USER_NAME         "test"
#define RIGTH_USER_PASSWD       "test"

#define OPPONENT_USER_NAME      "test1"
#define OPPONENT_USER_PASSWD    "test1"

#define RIGHT_TIME2STEP         DEFAULT_TIME2STEP
#define RIGHT_TIME2GAME         DEFAULT_TIME2GAME
#define RIGHT_MAXRATING         0 // TODO replace
#define RIGHT_MINRATING         0 // TODO replace

#define SHOW_FUNCTION_NAME      qDebug() << "================" << __FUNCTION__ << "================";


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
        for(int i=0; i<12; i++) {
                int rnd = 0;
                for(;;) {
                        rnd = (rand()%(int)'Z') + (int)'0';
                        if((rnd >= 48 && rnd <= 57) || (rnd >= 64 && rnd <= 90))
                                break;
                 }
                str += QChar(rnd);
        }

        return str;
}

#endif
