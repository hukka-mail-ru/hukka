#include "clientmsg.h"
#include "../header/deferror.h"
#include "../header/defservice.h"
#include <string.h>
#include <iostream>

#include <stdlib.h>
#include <sys/time.h>


using namespace std;

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

ClientMsg::ClientMsg()
{

}

ClientMsg::~ClientMsg()
{

}

void ClientMsg::InitError( uint32_t _nFrom, char _nCommand, char _nErr )
{

    if(_nErr == NOERR)
    {
        cout << "--- OUTGOING REPLY 'OK' ---  FROM: " << _nFrom <<"; COMMAND: " << ( uint32_t )(unsigned char) _nCommand << endl;
    }
    else
    {
        cout << "--- OUTGOING ERORR ---  FROM: " << _nFrom <<"; COMMAND: " << ( uint32_t )(unsigned char) _nCommand <<
                       ";  ERR: " << (uint32_t)(unsigned char)_nErr << endl;
    }

	m_vecDataMsg.resize( sizeof( MsgError ) );

	MsgError* pMsgError = (MsgError*)&m_vecDataMsg[0];

	pMsgError->m_cSign = 'Z';
	pMsgError->m_nSize = m_vecDataMsg.size()-sizeof( HeadMsg );
	pMsgError->m_cVersion = cntProtCurrVer;
	pMsgError->m_nFromTo = _nFrom;
	pMsgError->m_nCommand = _nCommand;
	pMsgError->m_nError = _nErr;
	pMsgError->m_nCRC = EvalCRC( &pMsgError->m_cVersion, &pMsgError->m_nCRC );
}

void ClientMsg::InitMsg( uint32_t _nTo, TVecChar _vecData )
{

    struct timeval tv;
    struct timezone tz;
    struct tm *tm;
    gettimeofday(&tv, &tz);
    tm=localtime(&tv.tv_sec);

    cout << "--- OUTGOING --- ("<< tm->tm_sec << "." << tv.tv_usec << ") "
                   << GlobalServer::commandToString( _vecData[0]) << "; ";
    for(int i=0; i<_vecData.size(); ++i)
        cout << (uint32_t)(unsigned char)_vecData[i] << " ";
    cout << endl;

	m_vecDataMsg.resize( sizeof( ExHeadMsg )+_vecData.size()+1 );

	ExHeadMsg* psExMsg = (ExHeadMsg*)&m_vecDataMsg[0];

	psExMsg->m_cSign = 'Z';
	psExMsg->m_nSize = m_vecDataMsg.size()-sizeof( HeadMsg );
	psExMsg->m_cVersion = cntProtCurrVer;
	psExMsg->m_nFromTo = _nTo;
	memcpy( &m_vecDataMsg[sizeof( ExHeadMsg )], &_vecData[0], _vecData.size() );
	m_vecDataMsg[m_vecDataMsg.size()-1] = EvalCRC( &psExMsg->m_cVersion, &m_vecDataMsg[m_vecDataMsg.size()-1] );
}

void ClientMsg::InitMsg( uint32_t _nTo, char _nCommand, TVecChar _vecData )
{

    struct timeval tv;
    struct timezone tz;
    struct tm *tm;
    gettimeofday(&tv, &tz);
    tm=localtime(&tv.tv_sec);

//    cout << "--- OUTGOING MSG --- ("<< tm->tm_sec << "." << tv.tv_usec << ") TO: " << _nTo <<
//                   ";  COMMAND: " << ( uint32_t ) _nCommand <<
//                   ";  DATA: ";
    for(int i=0; i<_vecData.size(); ++i)
        cout << (uint32_t)_vecData[i] << " ";
    cout << endl;

	m_vecDataMsg.resize( sizeof( CommandMsg )+_vecData.size()+1 );

	CommandMsg* psCommandMsg = (CommandMsg*)&m_vecDataMsg[0];

	psCommandMsg->m_cSign = 'Z';
	psCommandMsg->m_nSize = m_vecDataMsg.size()-sizeof( HeadMsg );
	psCommandMsg->m_cVersion = cntProtCurrVer;
	psCommandMsg->m_nFromTo = _nTo;
	psCommandMsg->m_nCommand = _nCommand;
	memcpy( &m_vecDataMsg[sizeof( CommandMsg )], &_vecData[0], _vecData.size() );
	m_vecDataMsg[m_vecDataMsg.size()-1] = EvalCRC( &psCommandMsg->m_cVersion, &m_vecDataMsg[m_vecDataMsg.size()-1] );
}

bool ClientMsg::ParseData( Buffer* _pBuffer, int& _nError )
{
	char* pErrMsg = 0;
	ExHeadMsg* psExHeadMsg;

	_nError = 0;

	for( char* pChar = _pBuffer->GetDataStart() ; pChar+sizeof( ExHeadMsg )+1 < _pBuffer->GetDataEnd() ; ++pChar )
	{
		psExHeadMsg = (ExHeadMsg*)pChar;

		_nError = _nError < ERRNOSIGN ? ERRNOSIGN : _nError;

		if( psExHeadMsg->m_cSign != 'Z' )
			continue;

		_nError = _nError < ERRVER ? ERRVER : _nError;

		if( psExHeadMsg->m_cVersion != cntProtCurrVer )
			continue;

		_nError = _nError < ERRTOBIG ? ERRTOBIG : _nError;

		if( psExHeadMsg->m_nSize >= 256*256 )
			continue;

		if( ( sizeof( HeadMsg )+psExHeadMsg->m_nSize ) > ( _pBuffer->GetDataEnd()-pChar ) )
		{
			_nError = ERRNOEND;
			pErrMsg = pErrMsg ? pErrMsg : pChar;
			continue;
		}

		_nError = _nError < ERRCRC ? ERRCRC : _nError;
		pErrMsg = pErrMsg ? pErrMsg : pChar+m_vecDataMsg.size();

		char cCRC = EvalCRC( &psExHeadMsg->m_cVersion, psExHeadMsg->m_nSize-1 );

		if( cCRC == pChar[sizeof( HeadMsg )+psExHeadMsg->m_nSize-1] )
		{
			m_vecDataMsg.assign( pChar, pChar+sizeof( HeadMsg )+psExHeadMsg->m_nSize );
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

uint32_t ClientMsg::GetTo() const
{
	ExHeadMsg* psExHeadMsg = (ExHeadMsg*)&m_vecDataMsg[0];
	return psExHeadMsg->m_nFromTo;
}

char ClientMsg::GetCommand() const
{
	CommandMsg* psCommandMsg = (CommandMsg*)&m_vecDataMsg[0];
	return psCommandMsg->m_nCommand;
}

TVecChar* ClientMsg::GetData( TypeData _typeData, TVecChar* _pvecData ) const
{
	int nPos = 0;

	switch( _typeData )
	{
	case etpExHead:	nPos = sizeof( ExHeadMsg );	break;
	case etpCommand:	nPos = sizeof( CommandMsg );	break;
	}

	_pvecData->assign( &m_vecDataMsg[ nPos ] , &m_vecDataMsg[ m_vecDataMsg.size()-1 ] );

    return _pvecData;
}

const TVecChar* ClientMsg::GetBuffMsg() const
{
	return &m_vecDataMsg;
}
