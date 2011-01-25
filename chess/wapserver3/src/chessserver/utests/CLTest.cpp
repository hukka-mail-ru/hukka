#include "CLTest.h"

#include <iostream>

#include "ChessLogic.h"

CLTest::CLTest()
{
}

CLTest::~CLTest()
{
}

void CLTest::run()
{
    if ( !GetStatus() )
    {
        std::cerr << "FAILED" << std::endl;
    }   
    else
    {
        std::cerr << "PASSED" << std::endl;
    }   

    if ( !setANDgetPosForDB() )
    {
        std::cerr << "FAILED" << std::endl;
    }   
    else
    {
        std::cerr << "PASSED" << std::endl;
    }   
    
    if ( !GetPosForClient() )
    {
        std::cerr << "FAILED" << std::endl;
    }        
    else
    {
        std::cerr << "PASSED" << std::endl;
    }        
        
    if ( !StepAnl() )
    {
        std::cerr << "FAILED" << std::endl;
    }   
    else
    {
        std::cerr << "PASSED" << std::endl;
    }               
}

bool CLTest::GetStatus()
{
    std::cerr << "---IChessLogic::GetStatus() test ";
    
    ChessLogic *pLogic = new ChessLogic;
    TVByte move;
    
    bool bRes = true;
    
    move.clear();
    move.push_back('g');
    move.push_back('2');
    move.push_back('g');
    move.push_back('4');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::Valid );  
    bRes = bRes && ( pLogic->GetStatus() == Normal );
    
    move.clear();
    move.push_back('e');
    move.push_back('7');
    move.push_back('e');
    move.push_back('5');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::Valid );  
    bRes = bRes && ( pLogic->GetStatus() == Normal );

    move.clear();
    move.push_back('f');
    move.push_back('2');
    move.push_back('f');
    move.push_back('3');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::Valid );  
    bRes = bRes && ( pLogic->GetStatus() == Normal );

    move.clear();
    move.push_back('d');
    move.push_back('8');
    move.push_back('h');
    move.push_back('4');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::Win );  
    bRes = bRes && ( pLogic->GetStatus() == Checkmate );
    
    delete pLogic;
    
    return bRes;
}

bool CLTest::setANDgetPosForDB()
{
    std::cerr << "---IChessLogic::setANDgetPosForDB() test ";
    
    ChessLogic *pLogic = new ChessLogic;
    TVByte move, position;
    
    bool bRes = true;
    
    move.push_back('e');
    move.push_back('2');
    move.push_back('e');
    move.push_back('4');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::Valid );
    
    for ( TVByte::const_iterator i = pLogic->GetPosForDB()->begin(); i != pLogic->GetPosForDB()->end(); ++i )
    {
        position.push_back( *i );
    }
    
    delete pLogic;
    
    pLogic = new ChessLogic;
    
    bRes =  bRes && pLogic->SetPos( position );
    
    move.clear();
    move.push_back('e');
    move.push_back('2');
    move.push_back('e');
    move.push_back('4');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::NotValid );
    
    delete pLogic;
    
    return bRes;
}

bool CLTest::GetPosForClient()
{
    std::cerr << "---IChessLogic::setANDgetPosForClient() test ";
        
    return false;
}

bool CLTest::StepAnl()
{
    std::cerr << "---IChessLogic::StepAnl() test ";

    ChessLogic *pLogic = new ChessLogic;
    TVByte move;
    
    bool bRes = true;
    
    move.push_back('e');
    move.push_back('2');
    move.push_back('e');
    move.push_back('4');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::Valid );  
    
    move.clear();
    move.push_back('x');
    move.push_back('2');
    move.push_back('e');
    move.push_back('4');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::NotValid );
    
    delete pLogic;
    
    pLogic = new ChessLogic;
    
    move.clear();
    move.push_back('g');
    move.push_back('2');
    move.push_back('g');
    move.push_back('4');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::Valid );  
    
    move.clear();
    move.push_back('e');
    move.push_back('7');
    move.push_back('e');
    move.push_back('5');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::Valid );  

    move.clear();
    move.push_back('f');
    move.push_back('2');
    move.push_back('f');
    move.push_back('3');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::Valid );  

    move.clear();
    move.push_back('d');
    move.push_back('8');
    move.push_back('h');
    move.push_back('4');
    bRes = bRes && ( pLogic->StepAnl( &move ) == IGameLogic::Win );  

    delete pLogic;
    
    return bRes;
}
