/*
 * Config.h
 *
 *  Created on: 24 May 2013
 *      Author: hukka
 */

#ifndef CONFIG_H_
#define CONFIG_H_

#include <map>

typedef std::string ConfigKey;
typedef std::string ConfigValue;

class Config
{
public:

	static ConfigValue GetConfigValue(const ConfigKey& key);

	static void ReadConfigFile(const std::string& configfile);

private:

	static std::map<ConfigKey, ConfigValue> mConfig;
};

#endif /* CONFIG_H_ */
