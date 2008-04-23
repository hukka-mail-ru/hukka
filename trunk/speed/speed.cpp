
#include <iostream>
#include <ping.hpp>
#include <debug.hpp>

using namespace std;

void PingIP(const char* ip)
{
    Pinger pinger;
    
    info << "Ping " << ip << "\n";

    switch(pinger.ping(ip))
    {
        case ERROR:
	    info << "Ping error.\n";
            break;

        case SILENCE:
	    
	    info << "Address doesn't respond.\n";
            break;
			
	case SUCCESS:
		   
	    info << "Elapsed time : " << pinger.getElapsedTime() << " microsec.\n";
	    info << "Bytes : " << pinger.getBytes() << "\n";
	    info << "Speed : " << 1000*pinger.getBytes() / pinger.getElapsedTime()
	        << " Kb/sec " << "\n";
	   break;

	default:
	       
	    error << "Logic error" << "\n";
    }

    info << "================================\n";
}




int main()
{
    PingIP("192.168.148.24");
    PingIP("192.168.148.103");
    PingIP("127.0.0.1");
    
    return 0;
}





