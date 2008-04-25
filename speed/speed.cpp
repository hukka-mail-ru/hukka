
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
void Ping(const char* ip)
{
    Pinger pinger;    
    debug << "Ping " << ip << endl;

    switch(pinger.ping(ip, 3))
    {
        case PING_ERROR:
	    error << "Ping error" << endl;
            break;

	case PING_SILENCE:
	    
	    info << "Address doesn't respond" << endl;
            break;
			
	case PING_SUCCESS:

	    info << "Ping time   : " << pinger.getElapsedTime() << " microsec." << endl;
	    info << "Bytes (ICMP): " << pinger.getBytes() << endl;
	    info << "Speed       : " << 1000*pinger.getBytes() / pinger.getElapsedTime()
		    << " Kb/sec " << endl;
	   break;

	default:
	       
	    error << "Logic error" << endl;
    }

}

// --------------------------------------------------------------------------------
//
//  Query Interfaces
//
// --------------------------------------------------------------------------------
void QueryInterfaces()
{
    NetInterfaces interfaces;

    if(interfaces.query() < 0)
    {
	error << "Error quering interfaces" << endl;
	return;
    }
    
    Interface* iface = interfaces.getFirst();
    while(iface)
    {
	info << "Name        : " << iface->mName << endl;
	info << "IP address  : " << iface->mAddress << endl;
	info << "Network mask: " << iface->mMask << endl;
	info << "MAC address : " << iface->mMacAddress << endl;

	Ping(iface->mAddress.c_str());

	info << "==============================" << endl;

	iface = interfaces.getNext();
    }
}



int main()
{
 //   Ping("192.168.148.24");
 //   Ping("192.168.148.103");
 //   Ping("127.0.0.1");

    QueryInterfaces();
   
    return 0;
}





