
#include <iostream>
#include <ping.hpp>
#include <debug.hpp>

using namespace std;

void PingIP(const char* ip)
{
    Pinger pinger;    
    info << "Ping " << ip << endl;

    switch(pinger.ping(ip, 3))
    {
        case ERROR:
	    error << "Ping error" << endl;
            break;

        case SILENCE:
	    
	    info << "Address doesn't respond" << endl;
            break;
			
	case SUCCESS:
		   
	    info << "Elapsed time : " << pinger.getElapsedTime() << " microsec.\n";
	    info << "Bytes : " << pinger.getBytes() << endl;
	    info << "Speed : " << 1000*pinger.getBytes() / pinger.getElapsedTime()
		    << " Kb/sec " << endl;
	   break;

	default:
	       
	    error << "Logic error" << endl;
    }

    info << "================================" << endl;
}




int main()
{
    PingIP("192.168.148.24");
    PingIP("192.168.148.103");
    PingIP("127.0.0.1");
    
    return 0;
}





