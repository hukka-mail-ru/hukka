#ifndef __IPCONFIG_HPP__
#define __IPCONFIG_HPP__

#include <vector>
#include <string>
#include <iostream>

#include <boost/shared_ptr.hpp>

struct Interface
{
    std::string mName;
    std::string mAddress;
    std::string mMask;
    std::string mMacAddress;
};



class IPConfig
{
    public:

	int queryInterfaces();
	
	/*
	const std::string& getAddress()    { return mAddress; }
	const std::string& getMask()       { return mMask; }
	const std::string& getMacAddress() { return mMacAddress; }
*/
    private:

	std::vector<boost::shared_ptr<Interface> > mInterfaces;

};

#endif
