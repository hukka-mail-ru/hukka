#include "CETest.h"

#include <iostream>

#include "../CHEOPSEngine.h"
#include "MoveLoader.h"

CETest::~CETest()
{
}

void CETest::run()
{        
    if ( !move() )
    {
        std::cerr << "FAILED" << std::endl;
    }   
    else
    {
        std::cerr << "PASSED" << std::endl;
    }   
    
    if ( !getResult() )
    {
        std::cerr << "FAILED" << std::endl;
    }        
    else
    {
        std::cerr << "PASSED" << std::endl;
    }        
        
    if ( !isWhiteStep() )
    {
        std::cerr << "FAILED" << std::endl;
    }   
    else
    {
        std::cerr << "PASSED" << std::endl;
    }           
    
    if ( !setANDgetPosition() )
    {
        std::cerr << "FAILED" << std::endl;
    }   
    else
    {
        std::cerr << "PASSED" << std::endl;
    }           
        

}

bool CETest::getResult()
{
    std::cerr << "---IChessEngine::getResult() test ";
    
    CHEOPSEngine* pEngine = new CHEOPSEngine();

    bool bRes = true;
    
    bRes = bRes && pEngine->move("g2g4");
    bRes = bRes && ( pEngine->getResult() == Normal );
    bRes = bRes && pEngine->move("e7e5");
    bRes = bRes && pEngine->move("f2f3");
    bRes = bRes && pEngine->move("d8h4");
    bRes = bRes && ( pEngine->getResult() == Checkmate );    
    
    delete pEngine;
    
    pEngine = new CHEOPSEngine(); 

    bRes = bRes && pEngine->move("d2d4");
    bRes = bRes && pEngine->move("c7c5");
    bRes = bRes && pEngine->move("g1f3");    
    bRes = bRes && pEngine->move("d8a5");
    bRes = bRes && ( pEngine->getResult() == Check );

    delete pEngine;
    
    return bRes;
}

bool CETest::isWhiteStep()
{
    std::cerr << "---IChessEngine::isWhiteStep() test ";
    
    CHEOPSEngine* pEngine = new CHEOPSEngine();
    
    bool bRes = true;
    
    bRes = bRes && pEngine->isWhiteStep();
    bRes = bRes && pEngine->move( "e2e4" ); 
    bRes = bRes && ( !pEngine->isWhiteStep() );        
        
    delete pEngine;
    return bRes;

}

bool CETest::move()
{
    std::cerr << "---IChessEngine::move() test "; 
    MoveLoader moveLoader;
    const MovesVectors *moves;

    CHEOPSEngine* pEngine = new CHEOPSEngine();

    bool bRes = true;

    if ( ! moveLoader.readFile( "./data/movevalid.txt") )
    {
        bRes = bRes && pEngine->move( "e2e4" );
    }
    else
    {
        
        moves = moveLoader.getMoves();
        
        StrVector::const_iterator i = moves->first.begin();
        
        for ( StrVector::const_iterator n = moves->second.begin(); n != moves->second.end(); ++n )
        {
            bRes = bRes && pEngine->move( *i );
            bRes = bRes && pEngine->move( *n );
            ++i;
        }       
    }
    
    if ( ! moveLoader.readFile( "./data/moveinvalid.txt") )
    {                
        bRes = bRes && ( !pEngine->move( "x2e4" ) );        
    }
    else
    {       
        
        moves = moveLoader.getMoves();
                
        StrVector::const_iterator i = moves->first.begin();
        
        for ( StrVector::const_iterator n = moves->second.begin(); n != moves->second.end(); ++n )
        {
            bRes = bRes && pEngine->move( *i );
            bRes = bRes && pEngine->move( *n );
            ++i;
            
            if ( !bRes )
            {
                bRes = true;
            }
        }
        
    }
    
    delete pEngine;

    return bRes;
}

bool CETest::setANDgetPosition()
{
    std::cerr << "---IChessEngine::setANDgetPosition() test ";
    CHEOPSEngine* pEngine = new CHEOPSEngine();
    
    const TVByte *curPos;
    TVByte actPos;
    if ( ! pEngine->move( "e2e4" ) )
    {
        delete pEngine;
        return false;
    }
    
    curPos = pEngine->getPosition();
    
    for ( TVByte::const_iterator i = curPos->begin(); i != curPos->end(); ++i )
    {
        actPos.push_back( *i );
    }
    
    delete pEngine;
    
    pEngine = new CHEOPSEngine();
    
    pEngine->setPosition( &actPos );
    
    if ( pEngine->move("e2e4") )
    {
        delete pEngine;
        return false;
    }
    
    if ( !pEngine->move("e7e5") )
    {
        delete pEngine;
        return false;
    }
    
    delete pEngine;
    return true;
   
}

