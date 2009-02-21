#ifndef MACROS_H_
#define MACROS_H_

#include <iostream> 
#include <sstream> 
#include <boost/shared_ptr.hpp> 


#define CLASSPTR(name) \
  class name; \
  typedef boost::shared_ptr< name > name##Ptr; \


#define TRY_BEGINS \
try \
{ \


#define TRY_RETHROW \
} \
catch(std::string& err) \
{ \
    std::ostringstream os; \
    os << "[" << __FUNCTION__ << "]->" << err; \
    throw os.str(); \
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


#define SHOW_FUNCTION_NAME \
std::cout << std::endl  << __FUNCTION__ << std::endl; \

#endif /*MACROS_H_*/
