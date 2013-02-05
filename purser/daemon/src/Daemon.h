/*
 * Daemon.h
 *
 *  Created on: 5 Feb 2013
 *      Author: hukka
 */

#ifndef DAEMON_H_
#define DAEMON_H_

#include <string>

class Daemon
{
public:
	Daemon(const std::string& pidfile): mPidfile(pidfile) {}
	virtual ~Daemon() {}

	virtual int Run() = 0;

	int Daemonize();

private:
	std::string mPidfile;
};


#endif /* DAEMON_H_ */
