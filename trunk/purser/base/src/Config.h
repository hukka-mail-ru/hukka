/*
 * Config.h
 *
 *  Created on: 6 Feb 2013
 *      Author: hukka
 */

#ifndef CONFIG_H_
#define CONFIG_H_

#include <string>

class Config {
public:
	static void ReadConfigFile(const std::string& configfile);

	static std::string Logfile;
	static unsigned Port;
};

#endif /* CONFIG_H_ */
