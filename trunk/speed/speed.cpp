
#include <iostream>
#include <ping.hpp>
#include <debug.hpp>

using namespace std;

void PingIP(const char* ip)
{
    Pinger pinger;
    
    cout << "Ping " << ip << "\n";

    switch(pinger.ping(ip))
    {
        case ERROR:
	    cout << "Ping error" << endl;
            break;

        case SILENCE:
	    
	    cout << "Address doesn't respond" << endl;
           break;
			
	case SUCCESS:
		   
	    cout << "Elapsed time : " << pinger.getElapsedTime() << " microsec" << endl;
	    cout << "Bytes : " << pinger.getBytes() << endl;
	    cout << "Speed : " << 1000000*pinger.getBytes() / pinger.getElapsedTime()
	        << " bytes/sec " << endl;
	   break;

	default:
	       
	    cout << "Logic error" << endl;
    }

    cout << "================================" << endl;
}




int main()
{
    PingIP("192.168.148.24");
    PingIP("192.168.148.103");
    PingIP("127.0.0.1");
    
    return 0;
}





