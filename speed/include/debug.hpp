#ifndef __DEBUG_HPP__
#define __DEBUG_HPP__

// --------------------------------------------------------------------------------
//  CHANGE THE BOOLEANS BELOW TO SWITCH LOGGING ON/OFF:
// -------------------------------------------------------------------------------
bool MessagesDebug = true;
bool MessagesInfo = true;
bool MessagesWarning = true;
bool MessagesError = true;

#include <iostream>
#include <string>
#include <memory>

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
std::auto_ptr<MessageHeader> HDR(const char* str)
{
    std::auto_ptr<MessageHeader> head(new MessageHeader());
    head->setHeader(str);
    return head;
}

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
	
	// Print header only if type = MessageHeader
	MessagePrinter& operator << (std::auto_ptr<MessageHeader> header)
	{
	       switch(mLevel)
	       {
		   case Debug:
		       if(MessagesDebug)
			   std::cout << header->getHeader() << "D ";
		       break;
		   case Info:
		       if(MessagesInfo)
			   std::cout << header->getHeader() << "I ";
		       break;
		   case Warning:
		       if(MessagesWarning)
			   std::cout << header->getHeader() << "W ";
		       break;
		   case Error:
		       if(MessagesError)
			   std::cout << header->getHeader() << "E ";
		       break;
		   default:
		       std::cerr << "Logic error" << std::endl;
		       break;
	       }
	       return *this;
        }

	// Print other staff
        template<class T>
        MessagePrinter& operator << (const T val)
        {
	    switch(mLevel)
	    {
		case Debug:
		    if(MessagesDebug)
			std::cout << val;
		    break;
		case Info:
		    if(MessagesInfo)
			std::cout << val;
		    break;
		case Warning:
		    if(MessagesWarning)
			std::cout << val;
		    break;
		case Error:
		    if(MessagesError)
			std::cout << val;
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
MessagePrinter debug(Debug);
MessagePrinter info(Info);
MessagePrinter warning(Warning);
MessagePrinter error(Error);


#endif
