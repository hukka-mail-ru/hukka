#ifndef __DEBUG_HPP__
#define __DEBUG_HPP__

// --------------------------------------------------------------------------------
//  CHANGE THE BOOLEANS BELOW TO SWITCH LOGGING ON/OFF:
// -------------------------------------------------------------------------------
extern bool EnableDebugLog;
extern bool EnableInfoLog;
extern bool EnableWarningLog;
extern bool EnableErrorLog;

#include <iostream>
#include <string>
#include <memory>

#include <color.hpp>

// --------------------------------------------------------------------------------
//  4 Message Levels
// --------------------------------------------------------------------------------
enum MessageLevel
{
    Debug,
    Info,
    Warning,
    Error
};

// --------------------------------------------------------------------------------
//  Proxy class
// --------------------------------------------------------------------------------
class MessageHeader
{
    public:
	void setHeader(const char* str) { mHeader = str; }
	const char* getHeader() { return mHeader.c_str(); }
    
    private:
	std::string mHeader;
};

// --------------------------------------------------------------------------------
//  MESSAGE HEADER. 
//  Example of usage:
//                     error << HDR("BAD001") << "Can't fuck the brain" << endl;
//            Result:
//                     BAD001E Can't fuck the brain
// --------------------------------------------------------------------------------
extern std::auto_ptr<MessageHeader> HDR(const char* str);


// --------------------------------------------------------------------------------
//  BIG MESSAGE HEADER.
//  Example of usage:
//                     error << BIG_HDR("BAD001") << "Can't fuck the brain" << endl;
//            Result:
//                     BAD001E [file.cpp; line 100] Can't fuck the brain
// --------------------------------------------------------------------------------
#define BIG_HDR(str) \
  HDR(str) << "[" << __FILE__ << "; line: " << __LINE__ << "] " 


// --------------------------------------------------------------------------------
//  MessagePrinter
// --------------------------------------------------------------------------------
class MessagePrinter
{

    public:

        MessagePrinter(MessageLevel level):
	   mLevel(level)
        {}

	// Print 'std::endl' and staff like that
	MessagePrinter& operator << (std::ostream& (*f)(std::ostream&) );

	// Print header only if type = MessageHeader
	MessagePrinter& operator << (std::auto_ptr<MessageHeader> header);

	// Print other staff
        template<class T>
        MessagePrinter& operator << (const T val)
        {
	    switch(mLevel)
	    {
		case Debug:
		    if(EnableDebugLog)
			std::cout << val;
		    break;
		case Info:
		    if(EnableInfoLog)
			std::cout << val;
		    break;
		case Warning:
		    if(EnableWarningLog)
			std::cout << val;
		    break;
		case Error:
		    if(EnableErrorLog)
			std::cerr << val;
		    break;
		default:
		    std::cerr << "Logic error" << std::endl;
		    break;
	    }
            return *this;
        }

    private:

	MessageLevel mLevel;   	

};

// --------------------------------------------------------------------------------
//  Global Variables
//  Example of usage:
//                    debug << "Debug message" << endl;
// --------------------------------------------------------------------------------
extern MessagePrinter debug;
extern MessagePrinter info;
extern MessagePrinter warning;
extern MessagePrinter error;


#endif
