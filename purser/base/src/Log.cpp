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

// by default, the log is printed to Console
const string OutputToConsole = "OutputToConsole";
string Log::mLogfile = OutputToConsole;



void Log::Write(const string& log)
{
	if(mLogfile == OutputToConsole)
	{
		cout << log;
	}
	else
	{
		fstream file;
		ios_base::openmode mode = ios_base::app | ios_base::out;
		file.open (mLogfile, mode);
		file << log;
		file.close();
	}
}

void Log::Write(const MyException& exception)
{
	Write("\033[1;31m\n"); // red
	Write(string("EXCEPTION!\n") + exception.what() + "\n");
	Write("\033[0m\n"); // black again
}

void Log::WriteBytes(const string& log)
{
	if(mLogfile == OutputToConsole)
	{
		for(char c: log)
		{
			cout << toascii(c) << " ";
		}
		cout << endl;
	}
	else
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
}


void Log::Clear()
{
	if(mLogfile != OutputToConsole)
	{
		remove(mLogfile.c_str());
	}
}


void Log::SetLogFile(const std::string& logfile)
{
	mLogfile = logfile;

	Clear();
}
