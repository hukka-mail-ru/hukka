#include <stdio.h>
#include <iostream>
using namespace std;


#include "Macros.h"
#include "System.h"
#include "UI.h"



int main(int argc, char *argv[]) 
{

    TRY_BEGINS;
    
    if(EDR_IsAppAlreadyRunning())
    {
        cout << "AppAlreadyRunning" << endl; 
        return -1;
    }

    UI ui;
    
    ui.startup();
    
    ui.handleEvents();
           
    TRY_CATCH;

    
    return 0;
}
