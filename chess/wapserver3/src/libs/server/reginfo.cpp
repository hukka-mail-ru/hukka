#include "reginfo.h"

RegInfo::RegInfo()
	:m_nID( 0 )
{

}

RegInfo::~RegInfo()
{

}

uint32_t RegInfo::GetID() const
{
	return m_nID;
}

void RegInfo::SetID( uint32_t _nID )
{
	m_nID = _nID;
}
