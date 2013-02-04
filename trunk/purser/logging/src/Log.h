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

#define PRINT_WHERE Log::Filename(__FILE__) + std::string(" ") \
                    + std::string("[") + std::to_string(__LINE__)  + std::string("] ") \
                    + std::string(__FUNCTION__) + std::string("\t")

#define PRINT_LOG 	Log() << PRINT_WHERE





class Log {
public:

	// helper: extracts file name from path name
	static std::string Filename(std::string pathname);

	static void SetLogFile(const std::string& logfile);

	static void Write(const std::string& log);
	static void Write(const MyException& exception);
	static void WriteBytes(const std::string& log);

	template<typename T>
	Log& operator<<( const T& t )
	{
		std::stringstream ss;
		ss << t;
		Write(ss.str());
		return *this;
	}

private:

	static void Clear();

	static std::string mLogfile;


};

#endif /* LOG_H_ */
