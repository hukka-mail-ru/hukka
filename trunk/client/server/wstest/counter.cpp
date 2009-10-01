#include "counter.h"

int CCounter::m_nCount = 0;

CCounter::CCounter()
{
	++m_nCount;
}

CCounter::~CCounter()
{
	--m_nCount;
}
