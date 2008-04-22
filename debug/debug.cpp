#include <debug.hpp>

	
// Print header only if type = MessageHeader
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
            std::cout << color(BLUE) << header->getHeader() << "I " << color(BLACK);
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
