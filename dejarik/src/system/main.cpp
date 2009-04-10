#include <stdio.h>
#include <iostream>
using namespace std;

#include "Game.h"
#include "UI.h"
#include "Macros.h"
#include "System.h"
#include "Windows.h"

#ifdef _WIN32
int WINAPI WinMain(HINSTANCE inst, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int cmdShow)
{   
    EDR_Instance = inst;
    EDR_CmdShow = cmdShow;
#else
int main(int argc, char *argv[]) 
{
#endif

    TRY_BEGINS;

    cout << "main" << endl; 
    
    if(EDR_IsAppAlreadyRunning())
    {
        cout << "AppAlreadyRunning" << endl; 
        return -1;
    }
       
    GamePtr game = GamePtr(new Game());    
    game->startup();
    
    cout << "game started" << endl; 
    
    UIPtr ui = UIPtr(new UI(game)); 
    ui->startup();
    
    cout << "ui started" << endl; 
    
    ui->handleEvents();
           
    TRY_CATCH;
   
    cout << "exit." << endl;
    
    return 0;
}
