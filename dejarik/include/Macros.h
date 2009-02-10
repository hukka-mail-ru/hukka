#ifndef MACROS_H_
#define MACROS_H_

#include <iostream> 
#include <boost/shared_ptr.hpp> 

#define TRY_BEGINS \
try \
{   

#define RETHROW(LOG) \
} \
catch(std::string& err) \
{ \
    std::string str = "["; \
    str += LOG; \
    str += "]->"; \
    str += err; \
    throw str; \
}

#define CATCH(LOG) \
} \
catch(std::string& err) \
{ \
std::cerr << "EXCEPTION in " << #LOG << std::endl << \
             "TRACE: "<< err << std::endl << std::endl;\
}

#define CLASSPTR(CLASS) \
class CLASS; \
typedef boost::shared_ptr<CLASS> CLASS##Ptr;

#endif /*MACROS_H_*/
