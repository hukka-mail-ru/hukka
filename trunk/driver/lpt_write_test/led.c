//============================================================================
// Name        : 111.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C, Ansi-style
//============================================================================

//#include <asm/io.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/io.h>
#include <stdio.h>

int main(int argc, char *argv[])
{
    char led = 3;

    if(argc > 1)
    {
        led = atoi(argv[1]);
    }

    if(iopl(3) < 0)
    {
        printf("Can't grant permissions for IO operations\n");
        return -1;
    }
                
    outb(led, 0x378);
    
    return 0;
} 
