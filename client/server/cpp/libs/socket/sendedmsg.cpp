#include "sendedmsg.h"

#include <iostream>

CSendedMsg::CSendedMsg()
{
}

CSendedMsg::~CSendedMsg()
{
}

void CSendedMsg::addData( char _chData )
{
    m_vecData.push_back( _chData );
}

void CSendedMsg::addData( uint32_t _nData )
{
    
  char data[sizeof(uint32_t)];
    
    memcpy(&data, &_nData, sizeof(uint32_t));
    
    for (int i = 0; i < sizeof(uint32_t); ++i)
    {
        m_vecData.push_back(data[i]);
    }

}

void CSendedMsg::addData( TVecByte* _pvecData )
{
    std::copy(_pvecData->begin(), _pvecData->end(), std::back_inserter(m_vecData));
}

void CSendedMsg::addData( TVecUINT* _pvecData )
{
    for (TVecUINT::const_iterator it = _pvecData->begin(); it != _pvecData->end(); ++it)
    {
#ifdef MYDEBUG
            std::cout << "CSendedMsg::addData input int: " <<*it << std::endl;
#endif
        for (int i = 0; i < sizeof(uint32_t); ++i)
        {
#ifdef MYDEBUG
            char a = *((char*)&(*it)+i);
            std::cout << (int) a << " ";
#endif
            m_vecData.push_back(*((char*)&(*it)+i));
        }
#ifdef MYDEBUG
        std::cout << std::endl;
#endif
    }
}

void CSendedMsg::getData( char* _pData, uint32_t& nSize )
{
    _pData = &m_vecData[0];
    nSize = m_vecData.size();
}
