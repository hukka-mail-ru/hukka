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

typedef std::string ConfigKey;
typedef std::string ConfigValue;

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

	ConfigValue GetConfigValue(const ConfigKey& key);

private:
	void ReadConfigFile(const std::string& configfile);

	std::string mPidfile;

	std::map<ConfigKey, ConfigValue> mConfig;
};


#endif /* DAEMON_H_ */
