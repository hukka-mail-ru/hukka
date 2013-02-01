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



class MyException : public std::exception
{
public :
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

private:
  std::string msg;
};


class ExceptionSocketError: public MyException
{
public:
	ExceptionSocketError(const std::string& what, const std::string& host, int port):
		MyException(MyException()
		    << what << ": host " << host << "; port " << port) {}
};


#endif /* EXCEPTION_H_ */
