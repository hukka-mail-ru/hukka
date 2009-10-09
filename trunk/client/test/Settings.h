#ifndef Settings_H_
#define Settings_H_

//#define RIGHT_SERVER_HOSTNAME    "wapserver3.wapportal.ru"
#define RIGHT_SERVER_HOSTNAME   "192.168.56.101"
#define RIGHT_SERVER_PORT       1234

#define RIGHT_PROXY_HOSTNAME    "proxy.t-systems.ru"
#define RIGHT_PROXY_PORT        3128
#define RIGHT_PROXY_TYPE        QNetworkProxy::HttpProxy // supports only outgoing TCP connections

#define RIGTH_USER_NAME         "tsrv"
#define RIGTH_USER_PASSWD       "tsrv"

#define SHOW_FUNCTION_NAME      qDebug() << "================" << __FUNCTION__ << "================";


#endif
