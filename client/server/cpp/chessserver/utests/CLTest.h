#ifndef CLTEST_H_
#define CLTEST_H_

class CLTest
{
public:
	CLTest();
	virtual ~CLTest();
	
	void run();
	
private:
    bool GetPosForClient();
    bool GetStatus();
    bool setANDgetPosForDB();
    bool StepAnl();
    
};

#endif /*CLTEST_H_*/
