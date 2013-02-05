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


#define WHERE_WHAT MyException() << where << "\t" << what

#define THROW_EX(EXTYPE)	throw EXTYPE << __WHERE__



class MyException
{
public :
  MyException(): mMsg(""), mInfo("") {};
  MyException( const MyException &rhs )
  {
    mMsg = rhs.mMsg;
    mInfo = rhs.mInfo;
  }

  virtual ~MyException() throw(){};

  virtual std::string what() const throw()
  {
	  std::string res = mMsg + "; " + mInfo;
      return res;
  }

  template<typename T>
  MyException& operator<<( const T& t )
  {
    std::stringstream ss;
    ss << t;
    mMsg +=ss.str();
    return *this;
  }

  void AddInfo(const std::string& text)
  {
	  mInfo += text;
  }

private:
  std::string mMsg;
  std::string mInfo;
};



class ExceptionSocketError: public MyException
{
public:
	ExceptionSocketError(const std::string& host, int port)
	{
	    std::stringstream ss;
	    ss << "host " << host << "; port " << port;
	    AddInfo(ss.str());
	}
};


class ExceptionProtocolError: public MyException
{
};

#endif /* EXCEPTION_H_ */
