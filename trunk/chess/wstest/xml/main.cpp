#include <stdio.h>
#include <iostream>
#include <nxml.h>

#include "setting.h"

void check( nxml_t* _pData, nxml_error_t _Err )
{
  if( _Err != NXML_OK )
  {
  	puts( nxmle_strerror( _pData, _Err ));
  	exit(1);
  }
}

nxml_data_t* add_element( nxml_t* _pData, nxml_data_t* _pRootElement, char* _pValue )
{
	nxml_error_t Err;
	
	nxml_data_t* pElement = nxmle_add_new( _pData, _pRootElement, &Err );
	check( _pData, Err );
	
	pElement->value = _pValue;
	pElement->type = NXML_TYPE_ELEMENT;
	
	return pElement;
}

nxml_data_t* add_text( nxml_t* _pData, nxml_data_t* _pRootElement, char* _pValue )
{
	nxml_error_t Err;
	
	nxml_data_t* pElement = nxmle_add_new( _pData, _pRootElement, &Err );
	check( _pData, Err );
	
	pElement->value = _pValue;
	pElement->type = NXML_TYPE_TEXT;
	
	return pElement;
}

nxml_attr_t* add_attribute( nxml_t* _pData, nxml_data_t* _pElement, char* _pName, char* _pValue )
{
	nxml_error_t Err;
	
	nxml_attr_t* pAttr = nxmle_add_attribute_new( _pData, _pElement, &Err);
	check( _pData, Err );

	pAttr->name = _pName;
	pAttr->value = _pValue;
	
	return pAttr;
}

void create()
{
	nxml_error_t Err;
	
	nxml_t* pData = nxmle_new_data( &Err );
	check( pData, Err );
	
	nxml_data_t* pRootElement = add_element( pData, 0, strdup( "WapServer" ) );
	add_attribute( pData, pRootElement, strdup( "ID" ), strdup( "03" ) );

	nxml_data_t* pElement = add_element( pData, pRootElement, strdup( "Ver" ) );
	add_text( pData, pElement, strdup( "03" ) );
	
	pElement = add_element( pData, pRootElement, strdup( "Port" ) );
	add_text( pData, pElement, strdup( "1234" ) );

	pElement = add_element( pData, pRootElement, strdup( "MySQLBase" ) );
	add_text( pData, pElement, strdup( "WapServer3Base" ) );

	Err = nxml_write_file( pData, "setting.xml" );
	check( pData, Err );
}

void parse()
{
	char* pBuf;
	nxml_error_t Err;
	
	nxml_t* pData = nxmle_new_data_from_file( "setting.xml", &Err );
	check( pData, Err );
	
//	unlink( "tmp.tmp" );
	
	nxml_data_t* pRootElement = nxmle_root_element( pData, &Err );
	check( pData, Err );
	
	fprintf( stdout, "%s\n", pRootElement->value );
	
	nxml_data_t* pElement = nxmle_find_element( pData, pRootElement, "Ver", &Err );
	check( pData, Err );
	
	pBuf = nxmle_get_string( pElement, &Err);
	check( pData, Err );
	
	fprintf( stdout, "\t%s = %s\n", pElement->value, pBuf );
	free( pBuf );

	pElement = nxmle_find_element( pData, pRootElement, "Port", &Err );
	check( pData, Err );
	
	pBuf = nxmle_get_string( pElement, &Err);
	check( pData, Err );
	
	fprintf( stdout, "\t%s = %s\n", pElement->value, pBuf );
	free( pBuf );
	
	pElement = nxmle_find_element( pData, pRootElement, "MySQLBase", &Err );
	check( pData, Err );
	
	pBuf = nxmle_get_string( pElement, &Err);
	check( pData, Err );
	
	fprintf( stdout, "\t%s = %s\n", pElement->value, pBuf );
	free( pBuf );	
}

int main(int argc, char* argv[])
{
	CSetting Setting;
	
	Setting.FindRootElement( "aaa" );
	fprintf( stdout, Setting.GetLastErrorAsString() );
	
	Setting.ReadFile( "setting.xml" );
	int nVer, nPort;
	if( Setting.GetElementAsInt( "WapServer", nVer ) == 0 )
		fprintf( stdout, "Ver = %d\n", nVer );
	if( Setting.GetElementAsInt( "Port", nPort ) == 0 )
		fprintf( stdout, "Port = %d\n", nPort );
	
	FILE* pFile = fopen( "setting.xml", "rt" );
	
	if( pFile == NULL )
	{
		fprintf( stdout, "Creating XML file\n" );
		create();
	}
	else
	{
		fprintf( stdout, "Parsing XML file\n" );	
		fclose( pFile );
		parse();	
	}

	return 0;
}
