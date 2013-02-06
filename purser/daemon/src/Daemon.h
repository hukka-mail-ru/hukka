/*
 * Daemon.h
 *
 *  Created on: 5 Feb 2013
 *      Author: hukka
 */

#ifndef DAEMON_H_
#define DAEMON_H_

#include <string>
#include <map>

class Daemon
{
public:
	Daemon(const std::string& pidfile, const std::string& configfile): mPidfile(pidfile)
	{
		ReadConfigFile(configfile);
	}

	virtual ~Daemon() {}

	virtual int Run() = 0;

	int Daemonize();

	std::string GetConfigValue(const std::string& key);

private:
	void ReadConfigFile(const std::string& configfile);

	std::string mPidfile;

	std::map<std::string, std::string> mConfig;
};


#endif /* DAEMON_H_ */
