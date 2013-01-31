#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <iostream>
#include <errno.h>
#include <string.h>

using namespace std;


int main(int argc, char *argv[])
{
    int listener = socket(AF_INET, SOCK_STREAM, 0);

    if (listener < 0) {
        std::cout << "Error socket create\n";
        cout << strerror(errno) << "\n";
        return 1;
    }

    struct sockaddr_in addr;
    addr.sin_family = AF_INET;
    addr.sin_port = htons(1233);
    addr.sin_addr.s_addr = htonl(INADDR_ANY);

    const int MAX_BIND_ATTEMPTS = 100;
    bool please_wait = true;

    for(int i=1; i<=MAX_BIND_ATTEMPTS; i++)
    {
    	int res = bind(listener, (struct sockaddr*)&addr, sizeof(addr));
		if (res < 0)
		{
			if(errno == EADDRINUSE && i < MAX_BIND_ATTEMPTS)
			{
				if(please_wait)
				{
					cout << "Please wait..." << endl;
					please_wait = false;
				}

				sleep(1);
				continue;
			}
			else
			{
				cout << "Error socket bind\n";
				cout << strerror(errno) << "\n";
				return 1;
			}
		}
		else
		{
			break;
		}
    }

    cout << "Ready" << endl;

    listen(listener, 1);

    while (true) {
        int client = accept(listener, NULL, NULL);

        if (client < 0) {
            std::cout << "Error socket accept\n";
            return 1;
        }

        const int MESSAGE_SIZE = 10;

        char buf[MESSAGE_SIZE] = {'0'};

        int bytes = recv(client, buf, MESSAGE_SIZE, MSG_WAITALL);
	    string str(buf);

	    if (str.length() > 10)
        {
           cout << "Message too long" << endl;
           continue;
        }

        std::cout << "Read: " << bytes << endl;

        str = "Echo: " + str;
        send(client, str.c_str(), str.length(), 0);

        if (str.length() > 3 && str.erase(3) == "GET") {
            std::string msg =
                "HTTP/1.0 200 Ok\r\n" \
                "Connection: close\r\n" \
                "Content-Type: text/html\r\n" \
                "\r\nHello, world!\n";

            send(client, msg.c_str(), msg.length(), 0);
        }

        close(client);


    }

    close(listener);

    return 0;
}
