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

#define PRINT_LOG 	Log() << Log::Filename(__FILE__) << " " \
                    << "[" << __LINE__  << "]" \
                    << __FUNCTION__ << "\t"





class Log {
public:

	static std::string Filename(std::string pathname);

	static void SetLogFile(const std::string& logfile);

	static void Write(const std::string& log);
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
