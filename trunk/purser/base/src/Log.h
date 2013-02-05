/*
 * Log.h
 *
 *  Created on: 31 Jan 2013
 *      Author: hukka
 */

#ifndef LOG_H_
#define LOG_H_

#include <string>
#include <sstream>

#include "MyException.h"
#include "Base.h"


#define PRINT_LOG 	Log() << __WHEN__ << " " << __WHERE__


class Log {
public:

	static void SetLogFile(const std::string& logfile);

	static void Write(const MyException& exception);
	static void WriteBytes(const std::string& bytes);

	template<typename T>
	Log& operator<<( const T& t )
	{
		std::stringstream ss;
		ss << t;
		Write(ss.str());
		return *this;
	}

private:

	static void Write(const std::string& str);
	static void Clear();

	static std::string mLogfile;


};

#endif /* LOG_H_ */
