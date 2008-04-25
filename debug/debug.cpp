#include <debug.hpp>

bool EnableDebugLog = false;
bool EnableInfoLog = true;
bool EnableWarningLog = true;
bool EnableErrorLog = true;

MessagePrinter debug(Debug);
MessagePrinter info(Info);
MessagePrinter warning(Warning);
MessagePrinter error(Error);

std::auto_ptr<MessageHeader> HDR(const char* str)
{
    std::auto_ptr<MessageHeader> head(new MessageHeader());
    head->setHeader(str);
    return head;
}

// --------------------------------------------------------------------------------
//	
// For std::endl
//
// --------------------------------------------------------------------------------
MessagePrinter&
MessagePrinter::operator << (std::ostream& (*f)(std::ostream&) )
{
    if(mLevel == Error && EnableErrorLog)
	std::cerr << std::endl;
    else
	if((mLevel == Debug && EnableDebugLog) ||
	(mLevel == Warning && EnableWarningLog) ||
	(mLevel == Info && EnableInfoLog))
		std::cout << std::endl;
	    
    return *this;
}

// --------------------------------------------------------------------------------
//	
// Print header only if type = MessageHeader
//
// --------------------------------------------------------------------------------
MessagePrinter& 
MessagePrinter::operator << (std::auto_ptr<MessageHeader> header)
{
    switch(mLevel)
    {
    case Debug:
        if(EnableDebugLog)
            std::cout << color(BLUE) << header->getHeader() << "D " << color(BLACK);
        break;
    case Info:
        if(EnableInfoLog)
            std::cout << color(BLACK) << header->getHeader() << "I " << color(BLACK);
        break;
    case Warning:
        if(EnableWarningLog)
            std::cout << color(YELLOW) << header->getHeader() << "W " << color(BLACK);
        break;
    case Error:
        if(EnableErrorLog)
            std::cout << color(RED) << header->getHeader() << "E " << color(BLACK);
        break;
    default:
        std::cerr << "Logic error" << std::endl;
        break;
    }
    return *this;
}
