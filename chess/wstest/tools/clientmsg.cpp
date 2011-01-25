#include "clientmsg.h"
#include "deferror.h"
#include <string.h>

const char cntProtCurrVer = 2;

char EvalCRC( const char* const _pB, int _nSize )
{
	char cCRC = 0;

	for( const char* pcChar = _pB ; pcChar < _pB+_nSize ; ++pcChar )
		cCRC ^= *pcChar;

	return cCRC;
}

char EvalCRC( const char* const _pB, const char* const _pE )
{
	return EvalCRC( _pB, _pE-_pB );
}

CClientMsg::CClientMsg()
{

}

CClientMsg::~CClientMsg()
{

}

void CClientMsg::InitError( uint32_t _nFrom, char _nCommand, char _nErr )
{
	m_vecDataMsg.resize( sizeof( SMsgError ) );

	SMsgError* psMsgError = (SMsgError*)&m_vecDataMsg[0];

	psMsgError->m_cSign = 'Z';
	psMsgError->m_nSize = m_vecDataMsg.size()-sizeof( SHeadMsg );
	psMsgError->m_cVersion = cntProtCurrVer;
	psMsgError->m_nFromTo = _nFrom;
	psMsgError->m_nCommand = _nCommand;
	psMsgError->m_nError = _nErr;
	psMsgError->m_nCRC = EvalCRC( &psMsgError->m_cVersion, &psMsgError->m_nCRC );
}

void CClientMsg::InitMsg( uint32_t _nTo, TVecChar _vecData )
{
	m_vecDataMsg.resize( sizeof( SExHeadMsg )+_vecData.size()+1 );

	SExHeadMsg* psExMsg = (SExHeadMsg*)&m_vecDataMsg[0];

	psExMsg->m_cSign = 'Z';
	psExMsg->m_nSize = m_vecDataMsg.size()-sizeof( SHeadMsg );
	psExMsg->m_cVersion = cntProtCurrVer;
	psExMsg->m_nFromTo = _nTo;
	memcpy( &m_vecDataMsg[sizeof( SExHeadMsg )], &_vecData[0], _vecData.size() );
	m_vecDataMsg[m_vecDataMsg.size()-1] = EvalCRC( &psExMsg->m_cVersion, &m_vecDataMsg[m_vecDataMsg.size()-1] );
}

void CClientMsg::InitMsg( uint32_t _nTo, char _nCommand, TVecChar _vecData )
{
	m_vecDataMsg.resize( sizeof( SCommandMsg )+_vecData.size()+1 );

	SCommandMsg* psCommandMsg = (SCommandMsg*)&m_vecDataMsg[0];

	psCommandMsg->m_cSign = 'Z';
	psCommandMsg->m_nSize = m_vecDataMsg.size()-sizeof( SHeadMsg );
	psCommandMsg->m_cVersion = cntProtCurrVer;
	psCommandMsg->m_nFromTo = _nTo;
	psCommandMsg->m_nCommand = _nCommand;
	memcpy( &m_vecDataMsg[sizeof( SCommandMsg )], &_vecData[0], _vecData.size() );
	m_vecDataMsg[m_vecDataMsg.size()-1] = EvalCRC( &psCommandMsg->m_cVersion, &m_vecDataMsg[m_vecDataMsg.size()-1] );
}

bool CClientMsg::ParseData( CBuffer* _pBuffer, int& _nError )
{
	char* pErrMsg = 0;
	SExHeadMsg* psExHeadMsg;

	_nError = 0;

	for( char* pChar = _pBuffer->GetDataStart() ; pChar+sizeof( SExHeadMsg )+1 < _pBuffer->GetDataEnd() ; ++pChar )
	{
		psExHeadMsg = (SExHeadMsg*)pChar;

		_nError = _nError < ERRNOSIGN ? ERRNOSIGN : _nError;

		if( psExHeadMsg->m_cSign != 'Z' )
			continue;

		_nError = _nError < ERRVER ? ERRVER : _nError;

		if( psExHeadMsg->m_cVersion != cntProtCurrVer )
			continue;

		_nError = _nError < ERRTOBIG ? ERRTOBIG : _nError;

		if( psExHeadMsg->m_nSize >= 256*256 )
			continue;

		if( ( sizeof( SHeadMsg )+psExHeadMsg->m_nSize ) > ( _pBuffer->GetDataEnd()-pChar ) )
		{
			_nError = ERRNOEND;
			pErrMsg = pErrMsg ? pErrMsg : pChar;
			continue;
		}

		_nError = _nError < ERRCRC ? ERRCRC : _nError;
		pErrMsg = pErrMsg ? pErrMsg : pChar+m_vecDataMsg.size();

		char cCRC = EvalCRC( &psExHeadMsg->m_cVersion, psExHeadMsg->m_nSize-1 );

		if( cCRC == pChar[sizeof( SHeadMsg )+psExHeadMsg->m_nSize-1] )
		{
			m_vecDataMsg.assign( pChar, pChar+sizeof( SHeadMsg )+psExHeadMsg->m_nSize );
			_pBuffer->RemoveData( pChar+m_vecDataMsg.size() );

			return true;
		}
	}

	switch( _nError )
	{
	case ERRNOSIGN:
	case ERRVER:
	case ERRTOBIG:	_pBuffer->RemoveData( _pBuffer->GetDataEnd()-11 );	break;
	case ERRCRC:
	case ERRNOEND:	_pBuffer->RemoveData( pErrMsg );			break;
	}

	return false;
}

uint32_t CClientMsg::GetTo() const
{
	SExHeadMsg* psExHeadMsg = (SExHeadMsg*)&m_vecDataMsg[0];
	return psExHeadMsg->m_nFromTo;
}

char CClientMsg::GetCommand() const
{
	SCommandMsg* psCommandMsg = (SCommandMsg*)&m_vecDataMsg[0];
	return psCommandMsg->m_nCommand;
}

TVecChar* CClientMsg::GetData( ETypeData _typeData, TVecChar* _pvecData ) const
{
	int nPos = 0;

	switch( _typeData )
	{
	case etpExHead:	nPos = sizeof( SExHeadMsg );	break;
	case etpCommand:	nPos = sizeof( SCommandMsg );	break;
	}

	_pvecData->assign( &m_vecDataMsg[ nPos ] , &m_vecDataMsg[ m_vecDataMsg.size()-1 ] );

    return _pvecData;
}

const TVecChar* CClientMsg::GetBuffMsg() const
{
	return &m_vecDataMsg;
}
