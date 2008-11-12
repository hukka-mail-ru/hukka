#include <sys/types.h>  // open
#include <sys/stat.h>   // open
#include <fcntl.h>      // open
#include <unistd.h>     // close
#include <sys/ioctl.h>  // ioctl


#include <iostream>

#include <hello.h>

using namespace std;

enum Error
{
    NO_ERROR,
    OPEN_ERROR,
    WRITE_ERROR,
    READ_ERROR,
    IOCTL_ERROR
};

int main()
{
    const char* dev = "/dev/hello";
    cout << "test" << endl;
    int fd = 0; // file descriptor

    try
    {
        // open
        fd = open(dev, O_RDWR);    
        if(fd < 0)
            throw OPEN_ERROR;
        
        // write
        const int size = 5;
        char buf[1024] = {'\0'};
                
        ssize_t res = write(fd, "Hukka", size);    
        if(res < 0)
            throw WRITE_ERROR;

        cout << "wrote " << res << " bytes" << endl;
        
        // ioctl stat
        long mem_size = 0;
        if(ioctl(fd, HELLO_IOCSTAT, &mem_size) < 0)
            throw IOCTL_ERROR;
            
        cout << "memory size " << mem_size << " bytes" << endl;
        
        // read
        res = read(fd, buf, size);
        if(res < 0)
            throw READ_ERROR;
        
        cout << "read " << size << " bytes: '" << buf  << "'" << endl;
        
        
        //ioctl format
        if(ioctl(fd, HELLO_IOCFORMAT, &mem_size) < 0)
            throw IOCTL_ERROR;
        
        // ioctl stat
        if(ioctl(fd, HELLO_IOCSTAT, &mem_size) < 0)
            throw IOCTL_ERROR;
        
        cout << "memory size " << mem_size << " bytes" << endl;

    }
    catch(Error err) 
    {
        
        switch(err)
        {
            case NO_ERROR:     break; 
            case OPEN_ERROR:   cout << "Can't open " << dev << endl; break;
            case WRITE_ERROR:  cout << "Can't write to " << dev << endl; break;
            case READ_ERROR:   cout << "Can't read from " << dev << endl; break;
            case IOCTL_ERROR:  cout << "Can't perform ioctl for " << dev << endl; break;
            default: break;
        }
        
    }

    // close
    if(fd < 0) // file was not open
        return -1;
    
    if(close(fd) < 0)
    {
        cout << "Can't close " << dev << endl;
        return -1;
    }        
    
    cout << "ok" << endl;
    return 0;
}
