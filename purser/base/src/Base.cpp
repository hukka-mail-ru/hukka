/*
 * Base.cpp
 *
 *  Created on: 5 Feb 2013
 *      Author: hukka
 */

#include <time.h>
#include <sys/time.h>

#include <algorithm>
#include <cstdlib>
#include <sstream>
#include <iomanip>

#include "Base.h"

using namespace std;

string Base::Filename(string pathname)
{
	string::iterator it = find_if(
			pathname.rbegin(),
		    pathname.rend(),
			  [] (char c)
			  {
				  return c == '/';
			  }
			).base();

	return string(it, pathname.end());
}


string Base::GetCurrentTime()
{
    struct timeval tv;
    struct timezone tz;
    struct tm *tm;
    gettimeofday(&tv, &tz);
    tm=localtime(&tv.tv_sec);

	stringstream ss;
	ss << setfill('0') << setw(2) << tm->tm_hour << ":" <<
	      setfill('0') << setw(2) << tm->tm_min << ":" <<
	      setfill('0') << setw(2) << tm->tm_sec << "."<< tv.tv_usec;

	return ss.str();
}


