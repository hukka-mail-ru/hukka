//============================================================================
// Name        : test.cpp
// Author      : Alexey Karelin
// Version     :
// Copyright   : 
// Description : Hello World in C, Ansi-style
//============================================================================

#include <stdio.h>
#include <stdlib.h>
#include <iostream>

#include "CETest.h"
#include "CLTest.h"


int main(void) 
{
      
    CETest CEtest;
    CLTest CLtest;
    
    CEtest.run();
    CLtest.run();
    
    
        
    return EXIT_SUCCESS;
}
