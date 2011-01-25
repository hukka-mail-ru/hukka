#ifndef CETEST_H_
#define CETEST_H_

#include "../IChessEngine.h"

class CETest
{
public:
	CETest(){};
	virtual ~CETest();
	
	void run();
	
private:
    
    bool getResult();
    bool isWhiteStep();
    bool move();
    bool setANDgetPosition();    
};

#endif /*CETEST_H_*/
