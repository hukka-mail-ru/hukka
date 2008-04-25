
#include <iostream>
#include <ping.hpp>
#include <debug.hpp>
#include <ipconfig.hpp>

using namespace std;

// --------------------------------------------------------------------------------
//
//  Ping 
//
// --------------------------------------------------------------------------------
void PingIP(const char* ip)
{
    Pinger pinger;    
    info << "Ping " << ip << endl;

    switch(pinger.ping(ip, 3))
    {
        case PING_ERROR:
	    error << "Ping error" << endl;
            break;

	case PING_SILENCE:
	    
	    info << "Address doesn't respond" << endl;
            break;
			
	case PING_SUCCESS:
		   
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

// --------------------------------------------------------------------------------
//
//  Query Interface
//
// --------------------------------------------------------------------------------
void Query(const char* interface)
{
    IPConfig ipconfig;

    if(ipconfig.queryInterfaces() >= 0)
    {
	/*
	info << "IP address  : " << ipconfig.getAddress() << endl;
	info << "Network mask: " << ipconfig.getMask() << endl;
	info << "MAC address : " << ipconfig.getMacAddress() << endl;
	*/
    }
    else
	error << "Error quering interfaces" << endl;
 }



int main()
{
  //  PingIP("192.168.148.24");
 //   PingIP("192.168.148.103");
  //  PingIP("127.0.0.1");

    Query("eth0");
   
    return 0;
}





