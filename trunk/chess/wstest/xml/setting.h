#ifndef SETTING_H_
#define SETTING_H_

#include "mysyslog.h"
#include <nxml.h>

class CSetting
{
public:
	CSetting();
	virtual ~CSetting();
	
	int		GetLastError()	const;
	char*	GetLastErrorAsString() const;
	
	int		ReadFile( char* = "" );
	
	int		FindRootElement( char* );
	int		FindChildElement( char* );
					
	int		GetElementAsInt( char*, int& );
private:

	void	Free();
private:
	
	nxml_t* 		m_pData;
	nxml_data_t* 	m_pRootElement;
	nxml_data_t* 	m_pCurrElement;
	nxml_error_t	m_nErr;
};

#endif /*SETTING_H_*/
