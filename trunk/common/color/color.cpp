#include <color.hpp>

using namespace std;
using namespace common;

bool common::EnableColorLog = false;

string common::color(int color)
{
    if(!EnableColorLog)
        return ""; 

	char command[13];

    if(color != UNDEFINED)
        sprintf(command, "%c[%d;%d;%dm", 0x1B, BRIGHT, color + 30, BLACK + 40);
    else
        sprintf(command, "%c[%d;%d;%dm", 0x1B, RESET, WHITE + 30, BLACK + 40);

    std::string s = command;
    return s;
}
