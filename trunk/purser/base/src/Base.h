/*
 * Base.h
 *
 *  Created on: 5 Feb 2013
 *      Author: hukka
 */

#ifndef BASE_H_
#define BASE_H_

#include <string>

#define __WHEN__	Base::GetCurrentTime()

#define __WHERE__   Base::Filename(__FILE__) << \
          " ["  << __LINE__  << "] " << __FUNCTION__<< "\t\t"


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
