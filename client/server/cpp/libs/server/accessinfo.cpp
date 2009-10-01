#include "accessinfo.h"
#include <string.h>

CAccessInfo::CAccessInfo( const char* _pcntLogin, const char* _pcntPassword ):
m_strLogin(_pcntLogin), 
m_strPassword(_pcntPassword)
{
//	m_pcLogin = new char[strlen( _pcntLogin )];
//	m_pcPassword = new char[strlen( _pcntPassword )];
//	memcpy( m_pcLogin, _pcntLogin, strlen( _pcntLogin ) );
//	memcpy( m_pcPassword, _pcntPassword, strlen( _pcntPassword ) );
}

CAccessInfo::~CAccessInfo()
{
//	delete[] m_pcLogin;
//	delete[] m_pcPassword;
}

const char* CAccessInfo::GetLogin() const
{
	return m_strLogin.c_str();
}

const char* CAccessInfo::GetPassword() const
{
	return m_strPassword.c_str();
}
