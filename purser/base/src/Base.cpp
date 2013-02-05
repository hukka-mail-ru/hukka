/*
 * Base.cpp
 *
 *  Created on: 5 Feb 2013
 *      Author: hukka
 */


#include <algorithm>
#include <cstdlib>
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



