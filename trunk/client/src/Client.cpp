#include <assert.h>
#include "Client.h"

ClientStatus Client::connect(const QNetworkProxy& proxy, const QString& hostName, quint16 port)
{
	assert(address != "");
	assert(proxy.type() == QNetworkProxy::NoProxy || proxy.type() == QNetworkProxy::HttpProxy);

	mSocket.setProxy(proxy);
//	mSocket.connectToHost (hostName, port);


}
