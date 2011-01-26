#include "accessinfo.h"
#include <string.h>

AccessInfo::AccessInfo( const char* _pcntLogin, const char* _pcntPassword ):
m_strLogin(_pcntLogin), 
m_strPassword(_pcntPassword)
{
//	m_pcLogin = new char[strlen( _pcntLogin )];
//	m_pcPassword = new char[strlen( _pcntPassword )];
//	memcpy( m_pcLogin, _pcntLogin, strlen( _pcntLogin ) );
//	memcpy( m_pcPassword, _pcntPassword, strlen( _pcntPassword ) );
}

AccessInfo::~AccessInfo()
{
//	delete[] m_pcLogin;
//	delete[] m_pcPassword;
}

const char* AccessInfo::GetLogin() const
{
	return m_strLogin.c_str();
}

const char* AccessInfo::GetPassword() const
{
	return m_strPassword.c_str();
}
