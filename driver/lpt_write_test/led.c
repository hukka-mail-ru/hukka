//============================================================================
// Name        : 111.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C, Ansi-style
//============================================================================

#include <asm/io.h>
#include <stdio.h>
#include <stdlib.h>
int main(int argc, char *argv[])
{
    char led = 0;

    if(argc > 1)
    {
	led = atoi(argv[1]);
    }

    iopl(3);
    outb(led, 0x378);
    printf("%d\n", led);	
    return 0;
} 
