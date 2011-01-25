#ifndef _ACCESSINFO_H
#define _ACCESSINFO_H

#include "../tools/mystr.h"

class CAccessInfo
{
public:

	CAccessInfo( const char*, const char* );
	~CAccessInfo();

	const char*	GetLogin()	const;
	const char*	GetPassword()	const;
private:

//	char*	m_pcLogin;
//	char*	m_pcPassword;
	CMyStr  m_strLogin;
	CMyStr  m_strPassword;
};

#endif
