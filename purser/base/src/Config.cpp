/*
 * Config.cpp
 *
 *  Created on: 24 May 2013
 *      Author: hukka
 */

#include "MyException.h"
#include "Config.h"

#include <iostream>
#include <sstream>
#include <fstream>
#include <string>

using namespace std;

std::map<ConfigKey, ConfigValue> Config::mConfig;

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
	    	mConfig[key] = value;
	    }
	  }
	}
}

ConfigValue Config::GetConfigValue(const ConfigKey& key)
{

	map<ConfigKey, ConfigValue>::iterator it = mConfig.find(key);

	if(it == mConfig.end())
	{
		THROW_EX(MyException()) << "Can't find config entry: '" << key <<"'";
	}

	return mConfig[key];
}
