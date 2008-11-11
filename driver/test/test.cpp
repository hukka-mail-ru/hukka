#include <sys/types.h>  // open
#include <sys/stat.h>   // open
#include <fcntl.h>      // open
#include <unistd.h>     // close
#include <sys/ioctl.h>  // ioctl


#include <iostream>

#include <hello.h>

using namespace std;


int main()
{
    const char* dev = "/dev/hello";
    cout << "test" << endl;

    // open
    int fd = open(dev, O_RDWR);    
    if(fd == -1)
    {
        cout << "can't open " << dev << endl;
        return -1;
    }
    
    // write
    const int size = 5;
    char buf[1024] = {'\0'};
    long param = 0;
    int result = 0;
    
    ssize_t res = write(fd, "Hukka", size);    
    cout << "wrote " << res << " bytes" << endl;
    if(res == -1)
    {
        cout << "can't write to " << dev << endl;
        return close(fd);
    }
    
    // read
    res = read(fd, buf, size);
    if(res == -1)
    {
        cout << "can't read from " << dev << endl;
        goto close;
    }    
    cout << "read " << size << " bytes: '" << buf  << "'" << endl;
    
    
    //ioctl
    result = ioctl(fd, HELLO_IOCFORMAT, param);
    if(result == -1)
    {
        cout << "can't do HELLO_IOCFORMAT for " << dev << endl;
        goto close;
    }    
    
    result = ioctl(fd, HELLO_IOCSTAT, param);
    if(result == -1)
    {
        cout << "can't do HELLO_IOCSTAT for " << dev << endl;
        goto close;
    }    
    cout << "memory size " << result << " bytes" << endl;
        
    
    
close:    
    // close
    if(close(fd) == -1)
    {
        cout << "can't close " << dev << endl;
        return -1;
    }
    cout << "ok" << endl;
    
    return 0;
}
