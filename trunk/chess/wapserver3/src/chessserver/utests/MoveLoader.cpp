#include "MoveLoader.h"

#include <fstream>
#include <iostream>
#include <stdio.h>
#include <stdint.h>

MoveLoader::MoveLoader()
{
}

MoveLoader::~MoveLoader()
{
}

bool MoveLoader::readFile( const String& _strFileName )
{
    std::ifstream inputFile( _strFileName.c_str() );
    StrVector vecLines;
    String strMove;
    
    m_MovesVectors.first.clear();
    m_MovesVectors.second.clear();
    
    const int size=15;
    char buffer[size], archWhite[5], archBlack[5];
    
    if ( !inputFile )
    {
        std::cerr << "open error" << std::endl; 
        return false;
    }
    
    while ( inputFile.getline( buffer, size ) )
    {
        
        if ( sscanf( buffer, "%s %s", archWhite, archBlack) != 2 )
        {
            std::cerr << "wrong format" << std::endl;
            return false;
        }
        
        m_MovesVectors.first.push_back( String( archWhite ) );
        m_MovesVectors.second.push_back( String( archBlack ) );
    }
    
    return true;
}

const MovesVectors* MoveLoader::getMoves() const
{
    return &m_MovesVectors;
}
