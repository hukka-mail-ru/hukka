#include "accessmanager.h"
#include <algorithm>
#include "../header/defserver.h"
#include "../header/deferror.h"
#include "onlinemanager.h"

AccessManager::AccessManager()
{
	m_pOnLineManager = OnLineManager::Instance();
}

AccessManager::~AccessManager()
{
	OnLineManager::FreeInst();
}

char AccessManager::GetAccessID( char _nCommand, const TVecChar& _vecData, uint32_t& _nID )
{
	TVecChar vecL, vecP;
	
	if( _nCommand != 1 )
		return ERRCOMMAND;

	if( !Parse2str( _vecData, &vecL, &vecP ) )
		return ERRMSG;

	return m_sqlTableUsers.IsUserReg( &vecL[0], &vecP[0], _nID );
}

char AccessManager::RegAccessID( char _nCommand, const TVecChar& _vecData, uint32_t& _nID )
{
	TVecChar vecL, vecP;
	
	if( _nCommand != 1 )
		return ERRCOMMAND;

	if( !Parse2str( _vecData, &vecL, &vecP ) )
		return ERRMSG;

	return m_sqlTableUsers.DoUserReg( &vecL[0], &vecP[0], _nID );
}

bool AccessManager::Parse2str( const TVecChar& _vecD, TVecChar* _pvecL, TVecChar* _pvecP )
{
	TVecChar::const_iterator It = std::find( _vecD.begin(), _vecD.end(), 0 );

	if( It == _vecD.end() )
		return false;

	_pvecL->assign( _vecD.begin(), It );
	_pvecL->push_back(0);
	_pvecP->assign( It+1, _vecD.end() );
	_pvecP->push_back(0);

	return true;
}
