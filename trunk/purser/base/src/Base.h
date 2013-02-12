/*
 * Base.h
 *
 *  Created on: 5 Feb 2013
 *      Author: hukka
 */

#ifndef BASE_H_
#define BASE_H_

#include <string>
#include <iomanip>

#define __WHEN__	Base::GetCurrentTime()

#define __WHERE__ "["  << std::right << std::setw(4) << __LINE__  << "] " << \
                 std::left << std::setw(16) << Base::Filename(__FILE__) << \
                 std::left << std::setw(16) << __FUNCTION__


#define GETTER_(TYPE, VAR) \
	TYPE Get ## VAR() const { return m ## VAR; }

#define SETTER_(TYPE, VAR) \
	void Set ## VAR(const TYPE& _ ## VAR) {  m ## VAR = _ ## VAR; }

#define GETTER_SETTER(TYPE, VAR) \
	GETTER_(TYPE, VAR) \
	SETTER_(TYPE, VAR)


class Base {
public:

	// helper: extracts file name from path name
	static std::string Filename(std::string pathname);

	static std::string GetCurrentTime();
};

#endif /* BASE_H_ */
