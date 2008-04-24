#ifndef __IPCONFIG_HPP__
#define __IPCONFIG_HPP__

#include <string>
#include <iostream>

class IPConfig
{
    public:

	int queryInterface(const char* interface);
	
	const char* getAddress() { return mAddress.c_str(); }
	const char* getMask() { return mMask.c_str(); }
	const char* getMacAddress() {  return mMacAddress.c_str(); }

    private:

	std::string mAddress;
	std::string mMask;
	std::string mMacAddress;

};

#endif
