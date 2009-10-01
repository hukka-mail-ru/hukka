#include "reginfo.h"

CRegInfo::CRegInfo()
	:m_nID( 0 )
{

}

CRegInfo::~CRegInfo()
{

}

uint32_t CRegInfo::GetID() const
{
	return m_nID;
}

void CRegInfo::SetID( uint32_t _nID )
{
	m_nID = _nID;
}
