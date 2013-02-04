/*
 * Exception.h
 *
 *  Created on: 1 Feb 2013
 *      Author: hukka
 */

#ifndef EXCEPTION_H_
#define EXCEPTION_H_

#include <string>
#include <sstream>
#include <stdexcept>

#define EXC_OUT MyException() << where << "\t"

class MyException : public std::exception
{
public :
  //MyException(const std::string& where): mWhere(where) {};
  MyException() {};
  MyException( const MyException &rhs )
  {
    msg = rhs.msg;
  }
  virtual ~MyException() throw(){};

  virtual const char * what() const throw()
  {
    return msg.c_str();
  }

  template<typename T>
  MyException& operator<<( const T& t )
  {
    std::stringstream ss;
    ss << t;
    msg +=ss.str();
    return *this;
  }

 // std::string where() const { return mWhere; }

private:
  std::string msg;
//  std::string mWhere;
};


class ExceptionSocketError: public MyException
{
public:
	ExceptionSocketError(const std::string& where, const std::string& what,
			             const std::string& host, int port):
		MyException(EXC_OUT << what << ": host " << host << "; port " << port) {}
};


class ExceptionProtocolError: public MyException
{
public:
	ExceptionProtocolError(const std::string& where, const std::string& what):
		MyException(EXC_OUT << what) {}
};

#endif /* EXCEPTION_H_ */
