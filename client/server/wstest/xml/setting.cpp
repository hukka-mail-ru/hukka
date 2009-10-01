#include "setting.h"


CSetting::CSetting()
	:m_pData( 0 ),
	 m_pRootElement( 0 ),
	 m_pCurrElement( 0 ),
	 m_nErr( NXML_OK )
{

}

CSetting::~CSetting()
{
	Free();
}

void CSetting::Free()
{
	if( m_pData )
		delete m_pData;
		
	m_nErr = NXML_OK;
	m_pData = 0;
}

int CSetting::GetLastError() const
{
	return m_nErr;
}

char* CSetting::GetLastErrorAsString() const
{
	return nxml_strerror( m_pData, m_nErr );
}

int CSetting::ReadFile( char* _pFileName )
{
	Free();
	
	m_pData = nxmle_new_data_from_file( _pFileName, &m_nErr );

	return m_nErr;
}

int CSetting::FindRootElement( char* _pcNameElement )
{
	if( m_pData == 0 )
		return m_nErr = NXML_ERR_DATA;
		
	m_pRootElement = nxmle_find_element( m_pData, 0, _pcNameElement, &m_nErr );
	
	if( m_nErr != NXML_OK )
		return  m_nErr;
		
	return m_nErr;
}
	
int CSetting::FindChildElement( char* _pcNameElement )
{
	
}

int CSetting::GetElementAsInt( char* _pcNameElement, int& _nParam )
{
	if( m_pData == 0 )
		return m_nErr = NXML_ERR_DATA;

	nxml_data_t* pRootElement = nxmle_root_element( m_pData, &m_nErr );
	
	if( m_nErr != NXML_OK )
		return  m_nErr;
	
	nxml_data_t* pElement = nxmle_find_element( m_pData, pRootElement, _pcNameElement, &m_nErr );
	
	if( m_nErr != NXML_OK )
		return m_nErr;
	
	char* pBuf = nxmle_get_string( pElement, &m_nErr);
	
	if( m_nErr != NXML_OK )
		return m_nErr;
		
	_nParam  = atoi( pBuf );
	
	return m_nErr;
}
