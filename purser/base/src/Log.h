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
#include <iomanip>

#include "MyException.h"
#include "Base.h"


#define PRINT_LOG 	{ std::stringstream ss; \
                      ss << __WHEN__ << " " << __WHERE__; \
	                  Log::Write(ss.str()); } \
                    Log()

#define PRINT_LOG_HI { std::stringstream ss; \
                      ss << "\033[1;34m" << __WHEN__ << " " << __WHERE__ << "\033[0m"; \
	                  Log::Write(ss.str()); } \
                    Log()

#define PRINT_EX(EXCEPTION) PRINT_LOG << "Catch exception!\n";  \
                            Log::Write(EXCEPTION);


class Log {
public:

	static void SetLogFile(const std::string& logfile);

	static void Write(const std::string& str);
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

	static void Clear();

	static std::string mLogfile;


};

#endif /* LOG_H_ */
