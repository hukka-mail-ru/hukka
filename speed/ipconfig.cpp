#include <arpa/inet.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/ioctl.h>
#include <netinet/in.h>
#include <net/if.h>

#include <ipconfig.hpp>

using namespace std;

IPConfig::IPConfig()
{
    // --------------------------------------------------------------------------------
    // Open socket
    // --------------------------------------------------------------------------------
    int socket_id = socket(AF_INET, SOCK_DGRAM, 0);

    // --------------------------------------------------------------------------------
    // Fill query structure
    // --------------------------------------------------------------------------------
    ifreq ifr;
    ifr.ifr_addr.sa_family = AF_INET; // I want to get an IPv4 IP address
    strncpy(ifr.ifr_name, "eth0", IFNAMSIZ-1); // I want IP address attached to "eth0"

    // --------------------------------------------------------------------------------
    // Make a query and memorize IP address
    // --------------------------------------------------------------------------------   
    ioctl(socket_id, SIOCGIFADDR, &ifr);
    sockaddr_in* addr = reinterpret_cast<sockaddr_in*>(&ifr.ifr_addr);
    mAddress = inet_ntoa(addr->sin_addr);
    
    // --------------------------------------------------------------------------------
    // Make a query and memorize IP mask
    // --------------------------------------------------------------------------------   
    ioctl(socket_id, SIOCGIFNETMASK, &ifr);
    addr = reinterpret_cast<sockaddr_in*>(&ifr.ifr_addr);
    mMask = inet_ntoa(addr->sin_addr);
     
    // --------------------------------------------------------------------------------
    // Close socket
    // --------------------------------------------------------------------------------        
    close(socket_id);

}
