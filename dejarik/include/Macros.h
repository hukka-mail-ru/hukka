#ifndef MACROS_H_
#define MACROS_H_

#include <iostream> 
#include <boost/shared_ptr.hpp> 

#define TRY_BEGINS \
try \
{   

#define TRY_RETHROW \
} \
catch(std::string& err) \
{ \
    std::string str = "["; \
    str += __FUNCTION__; \
    str += "]->"; \
    str += err; \
    throw str; \
}

#define TRY_CATCH \
} \
catch(std::string& err) \
{ \
std::cout << "EXCEPTION in " << __FUNCTION__ << std::endl << \
             "TRACE: "<< err << std::endl << std::endl;\
std::cerr << "EXCEPTION in " << __FUNCTION__ << std::endl << \
             "TRACE: "<< err << std::endl << std::endl;\
}

#define CLASSPTR(CLASS) \
class CLASS; \
typedef boost::shared_ptr<CLASS> CLASS##Ptr;

#define SHOW_FUNCTION_NAME \
std::cout << std::endl << __FUNCTION__ << std::endl; \

#endif /*MACROS_H_*/
