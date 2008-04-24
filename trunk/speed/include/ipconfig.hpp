#ifndef __IPCONFIG_HPP__
#define __IPCONFIG_HPP__

#include <string>
#include <iostream>

class IPConfig
{
    public:

	int queryInterface(const char* interface);
	
	const std::string& getAddress()    { return mAddress; }
	std::string& getMask()       { return mMask; }
	std::string& getMacAddress() { return mMacAddress; }

    private:

	std::string mAddress;
	std::string mMask;
	std::string mMacAddress;

};

#endif
