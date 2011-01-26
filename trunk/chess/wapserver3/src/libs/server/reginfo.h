#ifndef _REGINFO_H
#define _REGINFO_H

#include <sys/types.h>
#include <stdint.h>

class RegInfo
{
public:

	RegInfo();
	~RegInfo();

	uint32_t		GetID() const;
	void			SetID( uint32_t );
private:

	uint32_t		m_nID;
};

#endif
