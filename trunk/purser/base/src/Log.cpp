/*
 * Log.cpp
 *
 *  Created on: 31 Jan 2013
 *      Author: hukka
 */
#include <fstream>
#include <iostream>
#include <stdio.h>

#include "Log.h"

using namespace std;

string Log::mLogfile = "/var/log/log.txt";



void Log::Write(const string& log)
{
	fstream file;
	ios_base::openmode mode = ios_base::app | ios_base::out;
	file.open (mLogfile, mode);
	file << log;
	file.close();
}

void Log::Write(const MyException& exception)
{
	Write("\033[1;31m\n"); // red
	Write(string("EXCEPTION!\n") + exception.what() + "\n");
	Write("\033[0m\n"); // black again
}

void Log::WriteBytes(const string& log)
{
	fstream file;
	ios_base::openmode mode = ios_base::app | ios_base::out;
	file.open (mLogfile, mode);

	for(char c: log)
	{
		file << toascii(c) << " ";
	}
	file << endl;

	file.close();
}


void Log::Clear()
{
	remove(mLogfile.c_str());
}


void Log::SetLogFile(const std::string& logfile)
{
	mLogfile = logfile;
	cout << "Log: " << logfile << endl;

	Clear();
}
