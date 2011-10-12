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

CMyStr::CMyStr( const TVecChar * _pcStr )
{
    append( &_pcStr->at(0), _pcStr->size() );
}

CMyStr::CMyStr( const std::string& _cstdStr )
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

int CMyStr::toInt() const
{
    std::istringstream stream(*this);
    int number = 0;
    stream >> number;
    return number;
}

CMyStr Int2Str( int _nVal )
{
	std::stringstream streamNum;
	streamNum << _nVal;
	return streamNum.str();
}
