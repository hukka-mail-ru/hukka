#include <iostream>

#include <netdb.h>
#include <netinet/ip_icmp.h>
#include <netinet/ip.h>
#include <arpa/inet.h>
#include <sys/socket.h>

#include <sys/time.h>

#include <ping.hpp>
#include <debug.hpp>

using namespace std;

const unsigned	BYTES_TO_SEND = 60000;
const unsigned	IPLEN = 20;
const unsigned	MAXIPLEN = 60;
const unsigned	ICMP_MAXLEN = 65536 - MAXIPLEN - ICMP_MINLEN;

const int SEQUENCE = 12345;


// --------------------------------------------------------------------------------
//
//  Checksum
//
// --------------------------------------------------------------------------------

uint16_t in_cksum(uint16_t *addr, unsigned len)
{
    uint16_t answer = 0;
  /*
    * Our algorithm is simple, using a 32 bit accumulator (sum), we add
    * sequential 16 bit words to it, and at the end, fold back all the
    * carry bits from the top 16 bits into the lower 16 bits.
  */
    uint32_t sum = 0;
    while (len > 1)  {
	sum += *addr++;
	len -= 2;
    }

  // mop up an odd byte, if necessary
    if (len == 1) {
	*(unsigned char *)&answer = *(unsigned char *)addr ;
	sum += answer;
    }

  // add back carry outs from top 16 bits to low 16 bits
    sum = (sum >> 16) + (sum & 0xffff); // add high 16 to low 16
    sum += (sum >> 16); // add carry
    answer = ~sum; // truncate to 16 bits
    return answer;
}

// --------------------------------------------------------------------------------
//
//  Ping
//
// --------------------------------------------------------------------------------

int Pinger::ping(const char* address)
{

    info << HDR("PNG0001") << "Ping " << address << "\n";
    
    // --------------------------------------------------------------------------------
    // Open raw socket
    // --------------------------------------------------------------------------------
    int socket_id = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP);
    if (socket_id < 0)
    {
	perror("Error calling function 'socket'");	/* probably not running as superuser */
	return -1;
    }
    // --------------------------------------------------------------------------------
    // Create ICMP packet
    // --------------------------------------------------------------------------------
    char packet[ICMP_MINLEN] = {'\0'}; // empty packet
    
    icmp* icmp_header = reinterpret_cast<icmp*>(packet);   // 64 bits
    icmp_header->icmp_type = ICMP_ECHO; //  0 - 7 bits
    icmp_header->icmp_code = 0;         //  8 - 15 bits
    icmp_header->icmp_cksum = 0;        //  16 - 31 bits
    icmp_header->icmp_id = getpid();    //  32 - 47 bits
    icmp_header->icmp_seq = SEQUENCE;   //  48 - 63 bits
       
    // after filling all the fields, calculate checksum
    icmp_header->icmp_cksum = in_cksum(reinterpret_cast<uint16_t*>(packet),
				       sizeof(packet));
    
    // --------------------------------------------------------------------------------
    // "To" (set recepient address)
    // --------------------------------------------------------------------------------  
    sockaddr_in to;
    to.sin_family = AF_INET;
    to.sin_addr.s_addr = inet_addr(address);
    if (to.sin_addr.s_addr < 0)
    {
	error << BIG_HDR("")<< "Error calling function 'inet_addr'" << "\n";
	return -1;
    }

    // --------------------------------------------------------------------------------
    // Start timer
    // --------------------------------------------------------------------------------      
    timeval start;
    gettimeofday(&start, NULL);

    // --------------------------------------------------------------------------------
    // Send packet
    // --------------------------------------------------------------------------------  
    int bytes_sent = sendto(socket_id, packet, sizeof(packet) , 0,
			    reinterpret_cast<const sockaddr*>(&to), sizeof(to));
    if (bytes_sent < 0)
    {
	perror("Error calling function 'sendto'");
	return -1;
    }
    debug << BIG_HDR("") << "sent " <<  bytes_sent*8 << " bits " << "\n";
    mBytes = bytes_sent;
    
    // --------------------------------------------------------------------------------
    // Prepare to listen to echo
    // --------------------------------------------------------------------------------  
    // this descriptor needs to be verified if it's ready for reading
    fd_set rfds;
    FD_ZERO(&rfds);
    FD_SET(socket_id, &rfds);
    // time interval of the verifying
    timeval tv;
    tv.tv_sec = 1;
    tv.tv_usec = 0;
    
    // --------------------------------------------------------------------------------
    // Listen to echo
    // --------------------------------------------------------------------------------  
    bool cont = true;
    while(cont)
    {
	debug << BIG_HDR("") << "before select \n";

	
	if (select(socket_id + 1, &rfds, NULL, NULL, &tv) < 0) // ready for reading ?
	{
	    perror("Error calling function 'select'");
	    return -1;
	}
	else
	{
	    debug << BIG_HDR("") << "after select" << "\n";
	    
            // ---------------------------------------------------------------------------
            // Read a packet
            // ---------------------------------------------------------------------------
	    char packet[ICMP_MAXLEN]  = {'\0'};
	    sockaddr_in from;
	    socklen_t fromlen = 0;

	    debug << BIG_HDR("") << "before recvfrom" << "\n";
	    
	    int bytes_read = recvfrom(socket_id, packet, ICMP_MAXLEN, 0,
				      reinterpret_cast<sockaddr*>(&from), &fromlen);
	    debug << BIG_HDR("") << "after recvfrom" << "\n";
	    
	    if(bytes_read < 0)
	    {
		perror("Error calling function 'recvfrom'");
		return -1;
	    }
	    debug << BIG_HDR("") << "read " <<  bytes_read*8 << " bits \n";

	    // --------------------------------------------------------------------------------
	    // Check the IP header
	    // -------------------------------------------------------------------------------- 
	    ip* ip_header = reinterpret_cast<ip*>(&packet);
	    
	    if (static_cast<unsigned>(bytes_read) < (sizeof(ip_header) + ICMP_MINLEN))
	    {
		error << "packet too short (" << bytes_read*8  << " bits) from " << address << "\n";;
		return -1;
	    }
	    // --------------------------------------------------------------------------------
            // Now the ICMP part 
    	    // -------------------------------------------------------------------------------- 
	    icmp* icmp_header = reinterpret_cast<icmp*>(packet + IPLEN);
	    if (icmp_header->icmp_type == ICMP_ECHOREPLY)
	    {
		if (icmp_header->icmp_seq != SEQUENCE)
		{
		    debug << BIG_HDR("") << "ERR: received sequence # " << icmp_header->icmp_seq << "\n";
		    continue;
		}
		if (icmp_header->icmp_id != getpid())
		{
		    debug << BIG_HDR("") << "ERR: received id " << icmp_header->icmp_id << "\n";
		    continue;
		}
		cont = false;
	    }
	    else
	    {
		debug << BIG_HDR("") << "ERR: not an echo reply" << "\n";
		continue;
	    }
	    mBytes += bytes_read;
	}
    }
    // --------------------------------------------------------------------------------
    // Stop timer
    // --------------------------------------------------------------------------------      
    timeval end;
    gettimeofday(&end, NULL);
    
    mElapsed = 1000000*(end.tv_sec - start.tv_sec) + (end.tv_usec - start.tv_usec);
    if(mElapsed < 1)
	mElapsed = 1;

    return 0;
}
