#ifndef _ACCESSINFO_H
#define _ACCESSINFO_H

#include "../tools/mystr.h"

class AccessInfo
{
public:

	AccessInfo( const char*, const char* );
	~AccessInfo();

	const char*	GetLogin()	const;
	const char*	GetPassword()	const;
private:

//	char*	m_pcLogin;
//	char*	m_pcPassword;
	CMyStr  m_strLogin;
	CMyStr  m_strPassword;
};

#endif
