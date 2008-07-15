#include <iostream>
#include <fstream>
#include <vector>
//#include <algorithm>
//#include <ctype.h>

#include "parser.h"

using namespace std;
//using namespace boost;


void cutToSymbol(std::string& name, const char ch)
{
    size_t right = name.find(ch);
    if(right != string::npos) 
    {
        name = name.substr(0, right);
    }
}

bool Parser::getVarName(const std::string& line, std::string& name)
{
    name = line;

    cutToSymbol(name, ';');
    cutToSymbol(name, ',');
    cutToSymbol(name, '=');

    size_t right = name.find_last_not_of(' ');
    name = name.substr(0, right + 1);

    size_t left = name.find_last_of(' ');
    name = name.substr(left + 1, name.size());

        
    return true;
}




