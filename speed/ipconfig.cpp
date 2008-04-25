#include <sstream>
#include <iomanip>

#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>

#include <ipconfig.hpp>

#define MAX_NUM_IFREQ 512

using namespace std;
using namespace boost;

int
NetInterfaces::query()
{
    // --------------------------------------------------------------------------------
    // Open socket
    // --------------------------------------------------------------------------------
    int socket_id = socket(AF_INET, SOCK_DGRAM, 0);
    if (socket_id < 0)
    {
	perror("Error calling function 'socket'");	
	return -1;
    }
    
    // --------------------------------------------------------------------------------
    // Obtain interfaces names
    // -------------------------------------------------------------------------------- 
    ifconf ifc;
    ifreq  buffer[MAX_NUM_IFREQ];
    ifc.ifc_len = sizeof(buffer);
    ifc.ifc_buf = reinterpret_cast<char*>(buffer);
    
    if ( ioctl(socket_id, SIOCGIFCONF, &ifc) < 0)
    {
	perror("Error calling function 'ioctl SIOCGIFCONF': ");
	return -1;
    }
       
    int inum = ifc.ifc_len / sizeof(ifreq); // number of interfaces
    ifreq* pIfr = ifc.ifc_req;
    for (int i = 0 ; i < inum; pIfr++, i++ )
    {
	if (pIfr->ifr_addr.sa_family != AF_INET) // I want only inet interfaces
	    continue;

        shared_ptr<Interface> iface (new Interface());
        iface->mName = pIfr->ifr_name;
        mInterfaces.push_back(iface); // memorize the interface
    }


    // --------------------------------------------------------------------------------
    // Query the interfaces 
    // --------------------------------------------------------------------------------
    for(unsigned i = 0; i < mInterfaces.size(); i++)
    {
	ifreq ifr;
        ifr.ifr_addr.sa_family = AF_INET; // I want to get an IPv4 IP address
	strncpy(ifr.ifr_name, mInterfaces[i]->mName.c_str(), IFNAMSIZ-1); // I want IP address attached to interface

	// --------------------------------------------------------------------------------
	// Make a query and memorize IP address
	// --------------------------------------------------------------------------------
	if (ioctl(socket_id, SIOCGIFADDR, &ifr) < 0)
	{
	    perror("Error calling function 'ioctl'");	
	    return -1;
	}
	sockaddr_in* addr = reinterpret_cast<sockaddr_in*>(&ifr.ifr_addr);
	mInterfaces[i]->mAddress = inet_ntoa(addr->sin_addr);
	
	// --------------------------------------------------------------------------------
	// Make a query and memorize mask
	// --------------------------------------------------------------------------------
	if (ioctl(socket_id, SIOCGIFNETMASK, &ifr) < 0)
	{
	    perror("Error calling function 'ioctl'");
	    return -1;
	}
	addr = reinterpret_cast<sockaddr_in*>(&ifr.ifr_addr);
	mInterfaces[i]->mMask = inet_ntoa(addr->sin_addr);
	
	// --------------------------------------------------------------------------------
	// Make a query and memorize MAC address
	// --------------------------------------------------------------------------------
	if (ioctl(socket_id, SIOCGIFHWADDR, &ifr) < 0)
	{
	    perror("Error calling function 'ioctl'");
	    return -1;
	}

	ostringstream stream;
	stream.setf(ios::hex,ios::basefield); // clean format and set 'hex' only
	stream.setf(ios::uppercase);
	stream.fill('0');

	stream << 
		setw(2) << (short)(unsigned char)ifr.ifr_hwaddr.sa_data[0] << ":" << 
		setw(2) << (short)(unsigned char)ifr.ifr_hwaddr.sa_data[1] << ":" << 
		setw(2) << (short)(unsigned char)ifr.ifr_hwaddr.sa_data[2] << ":" << 
		setw(2) << (short)(unsigned char)ifr.ifr_hwaddr.sa_data[3] << ":" << 
		setw(2) << (short)(unsigned char)ifr.ifr_hwaddr.sa_data[4] << ":" << 
		setw(2) << (short)(unsigned char)ifr.ifr_hwaddr.sa_data[5];
 	
	mInterfaces[i]->mMacAddress = stream.str();
    }
    
    // --------------------------------------------------------------------------------
    // Close socket
    // --------------------------------------------------------------------------------        
    if (close(socket_id) < 0)
    {
	perror("Error calling function 'close'");	
	return -1;
    }

    return 0;
}
