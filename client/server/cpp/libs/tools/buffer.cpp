#include "buffer.h"
#include <string.h>

const int cntBufSize = 256*256;

CBuffer::CBuffer()
	:m_nBuffSize( cntBufSize ),
	 m_nDataSize( 0 )
{
	m_pBuff = new char[cntBufSize];
}

CBuffer::~CBuffer()
{
	delete[] m_pBuff;
}

char* CBuffer::GetDataStart()
{
	return m_pBuff;
}

char* CBuffer::GetDataEnd()
{
	return m_pBuff+m_nDataSize;
}

int CBuffer::DataSize() const
{
	return m_nDataSize;
}

int CBuffer::FreeSize() const
{
	return m_nBuffSize-m_nDataSize;
}

void CBuffer::IncBuffer()
{
	char* pNewBuff = new char[m_nBuffSize+cntBufSize];

	memcpy( pNewBuff, m_pBuff, m_nDataSize );

	delete[] m_pBuff;

	m_pBuff = pNewBuff;
	m_nBuffSize += cntBufSize;
}

void CBuffer::AddDataSize( int _nSize )
{
	m_nDataSize += _nSize;
}

void CBuffer::RemoveData( char* _pPos )
{
	if( GetDataStart() == _pPos )
		return;

	if( ( GetDataStart() < _pPos  ) && ( _pPos <= GetDataEnd() ) )
	{
		m_nDataSize = GetDataEnd()-_pPos;
		if( m_nDataSize )
		{
			char* pNewBuff = new char( m_nDataSize );
			memcpy( pNewBuff, _pPos, m_nDataSize );
			bzero( m_pBuff, m_nBuffSize );
			memcpy( m_pBuff, pNewBuff, m_nDataSize );
			delete[] pNewBuff;
		}
	}
	else
	{
		bzero( m_pBuff, m_nBuffSize );
		m_nDataSize = 0;
	}
}
