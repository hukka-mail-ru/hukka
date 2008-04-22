#include <debug.hpp>

bool MessagesDebug = true;
bool MessagesInfo = true;
bool MessagesWarning = true;
bool MessagesError = true;

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
// Print header only if type = MessageHeader
//
// --------------------------------------------------------------------------------
MessagePrinter& 
MessagePrinter::operator << (std::auto_ptr<MessageHeader> header)
{
    switch(mLevel)
    {
    case Debug:
        if(MessagesDebug)
            std::cout << color(BLUE) << header->getHeader() << "D " << color(BLACK);
        break;
    case Info:
        if(MessagesInfo)
            std::cout << color(BLACK) << header->getHeader() << "I " << color(BLACK);
        break;
    case Warning:
        if(MessagesWarning)
            std::cout << color(YELLOW) << header->getHeader() << "W " << color(BLACK);
        break;
    case Error:
        if(MessagesError)
            std::cout << color(RED) << header->getHeader() << "E " << color(BLACK);
        break;
    default:
        std::cerr << "Logic error" << std::endl;
        break;
    }
    return *this;
}
