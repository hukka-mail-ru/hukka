//============================================================================
// Name        : 111.cpp
// Author      : 
// Version     :
// Copyright   : Your copyright notice
// Description : Hello World in C, Ansi-style
//============================================================================

#include <asm/io.h>
int main()
{
    iopl(3);
    outb(0x0,0x378);
} 
