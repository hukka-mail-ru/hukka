/*
 * Log.cpp
 *
 *  Created on: 31 Jan 2013
 *      Author: hukka
 */
#include <fstream>
#include <stdio.h>

#include "Log.h"

using namespace std;

string Log::mLogfile = "/home/hukka/devel/purser/daemon/log.txt";


void Log::Write(const string& log)
{
	fstream file;
	ios_base::openmode mode = ios_base::app | ios_base::out;
	file.open (mLogfile, mode);
	file << log << endl;
	file.close();
}


void Log::Clear()
{
	if(remove(mLogfile.c_str()) != 0)
	{
	    Write("Error deleting file: " + mLogfile);
	}
}


void Log::SetLogFile(const std::string& logfile)
{
	mLogfile = logfile;
}
