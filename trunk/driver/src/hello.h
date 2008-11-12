
#ifndef HELLO_H
#define HELLO_H

// ioctl parameters
/* Use 'h' as magic number */
#define HELLO_IOC_MAGIC  'h'

#define HELLO_IOCFORMAT _IOW(HELLO_IOC_MAGIC, 0, int)
#define HELLO_IOCSTAT    _IO(HELLO_IOC_MAGIC, 0)


#endif
