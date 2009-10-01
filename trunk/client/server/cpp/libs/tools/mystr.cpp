#include <sstream>
#include "mystr.h"

CMyStr::CMyStr()
{

}

CMyStr::CMyStr( int _nVal )
{
	append( Int2Str( _nVal ) );
}

CMyStr::CMyStr( const char* _pcStr )
{
	append( _pcStr );
}

CMyStr::CMyStr( std::string _cstdStr )
{
	append( _cstdStr );
}

CMyStr::~CMyStr()
{

}

CMyStr CMyStr::operator+( CMyStr& _myStr ) const
{
	CMyStr myStr;

	myStr.assign( begin(), end() );
	myStr.append( _myStr );

	return myStr;
}

CMyStr Int2Str( int _nVal )
{
	std::stringstream streamNum;
	streamNum << _nVal;
	return streamNum.str();
}
