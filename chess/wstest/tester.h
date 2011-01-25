#ifndef TESTER_H_
#define TESTER_H_

#include "counter.h"
#include "tools/mythread.h"
#include "mysocket.h"

typedef std::string stlstr;

class CTester : public CCounter, public CMyThread
{
public:
	CTester();
		
	virtual ~CTester();

private:
	int		Run();
	
	bool	sendMsg( stlstr* _pstrMsg );
	
	bool	testConnect();
	bool	testAuthorize();
	bool	testSendingCmd();
	
	CMySocket	m_Socket;
	
	std::string	m_strLogin;
	std::string m_strPassword;
	
	bool		m_bisContinue;
};

#endif /*TESTER_H_*/
