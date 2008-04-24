#include <sstream>
#include <iomanip>

#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>

#include <ipconfig.hpp>

using namespace std;

int
IPConfig::queryInterface(const char* interface)
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
    // Fill query structure
    // --------------------------------------------------------------------------------
    ifreq ifr;
    ifr.ifr_addr.sa_family = AF_INET; // I want to get an IPv4 IP address
    strncpy(ifr.ifr_name, interface, IFNAMSIZ-1); // I want IP address attached to interface

    // --------------------------------------------------------------------------------
    // Make a query and memorize IP address
    // --------------------------------------------------------------------------------   
    if (ioctl(socket_id, SIOCGIFADDR, &ifr) < 0)
    {
	perror("Error calling function 'ioctl'");	
	return -1;
    }
    sockaddr_in* addr = reinterpret_cast<sockaddr_in*>(&ifr.ifr_addr);
    mAddress = inet_ntoa(addr->sin_addr);
    
    // --------------------------------------------------------------------------------
    // Make a query and memorize mask
    // --------------------------------------------------------------------------------   
    if (ioctl(socket_id, SIOCGIFNETMASK, &ifr) < 0)
    {
	perror("Error calling function 'ioctl'");
	return -1;
    }
    addr = reinterpret_cast<sockaddr_in*>(&ifr.ifr_addr);
    mMask = inet_ntoa(addr->sin_addr);
     
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
    stream << setw(2) <<
	    (int)(unsigned char)ifr.ifr_hwaddr.sa_data[0] << ":" <<
	    (int)(unsigned char)ifr.ifr_hwaddr.sa_data[1] << ":" <<
	    (int)(unsigned char)ifr.ifr_hwaddr.sa_data[2] << ":" <<
	    (int)(unsigned char)ifr.ifr_hwaddr.sa_data[3] << ":" <<
	    (int)(unsigned char)ifr.ifr_hwaddr.sa_data[4] << ":" <<
	    (int)(unsigned char)ifr.ifr_hwaddr.sa_data[5];
	    
    mMacAddress = stream.str();

    
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
