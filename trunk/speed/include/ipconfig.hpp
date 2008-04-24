#ifndef __IPCONFIG_HPP__
#define __IPCONFIG_HPP__

#include <string>

class IPConfig
{
    public:

	IPConfig();   
	const char* getAddress() { return mAddress.c_str(); }
	const char* getMask() { return mMask.c_str(); }

    private:

	std::string mAddress;
	std::string mMask;

};

#endif
