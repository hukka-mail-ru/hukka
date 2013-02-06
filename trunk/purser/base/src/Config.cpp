/*
 * Config.cpp
 *
 *  Created on: 6 Feb 2013
 *      Author: hukka
 */

#include "Config.h"
#include "MyException.h"

#include <string>
#include <sstream>
#include <fstream>
#include <iostream>

#include <stdlib.h>

using namespace std;

std::string Config::Logfile = "/var/log/default.log";
unsigned Config::Port = 0;

void Config::ReadConfigFile(const string& configfile)
{

	ifstream file;
	file.open(configfile.c_str());

	if(!file)
	{
		THROW_EX(MyException()) << "Can't open config file: " << configfile;
	}

	string line;
	while( getline(file, line) )
	{
	  istringstream iss_line(line);
	  string key;
	  if( getline(iss_line, key, '=') )
	  {
	    string value;
	    if( getline(iss_line, value) )
	    {
	    	if(key == "port")
	    	{
	    		Port = atoi(value.c_str());
	    	}
	    	else if (key == "logfile")
	    	{
	    		Logfile = value;
	    	}
	    }
	  }
	}
}
