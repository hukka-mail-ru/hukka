
#include <iostream>
#include <ping.hpp>


using namespace std;

int PingIP(const char* ip)
{
    Pinger pinger;
    
    if(pinger.ping(ip) < 0)
	return -1;
    
    cout << "Elapsed time : " << pinger.getElapsed() << " microsec" << endl;
    cout << "Bytes : " << pinger.getBytes() << endl;
    
    cout << "Speed : " << 1000000*pinger.getBytes() / pinger.getElapsed()
	    << " bytes/sec " << endl;

    cout << "================================" << endl;
    return 0;
    
}




int main()
{
    PingIP("192.168.148.103");
    PingIP("192.168.148.24");
    PingIP("127.0.0.1");
    
    return 0;
}





