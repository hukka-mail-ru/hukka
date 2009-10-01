#include "gammonlogic.h"
#include <iostream>

#ifndef WIN32
#include <syslog.h>
#else
#define syslog(fl, text) 
#endif



CGammonLogic::CGammonLogic()
{
    m_vecPos.reserve(1024);
    m_vecPosForClient.resize(SIZE);

    StartNewGame();
}


CGammonLogic::~CGammonLogic()
{
    
}

/*!
    \fn CGammonLogic::StepAnl( TVByte* _pvecbtPos )
 */
IGameLogic::StepRes CGammonLogic::StepAnl( TVByte* _pvecbtPos )
{
    /// @todo implement me

    BG::MoveDesc move;

    if ( ! CheckSide(_pvecbtPos->at(8)) )
	{
		syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl CheckSide(_pvecbtPos->at(8)) == false");
	    return NotValid;
	}

    for(int i = 0; i < 8; ++i)
    {
        if ( m_pAction->side == BG::Match::X )
            move.desc[i] = _pvecbtPos->at(i);
        else if ( _pvecbtPos->at(i) > 0 )
            move.desc[i] = 25 - _pvecbtPos->at(i);
        else
            move.desc[i] = -1;
    }
    
    syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #1");

    if ( m_pAction->actionType == BG::Match::Action::ROLLS )
    {
    	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #2");
        if ( m_pAction->side == BG::Match::X )
        {
        	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #2.1");
            if ( !m_pMatch->opPlays(move) )
            {
            	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #2.1.1");
            	return NotValid;
            }
        }
        else
        {
        	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #2.2");
            if ( !m_pMatch->doMove(*m_pAction, move) )
            {
            	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #2.2.1");
            	return NotValid;
            }
        }
    }
    else if ( m_pAction->actionType == BG::Match::Action::DOUBLES )
    {
    	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #3");
    }
    else if ( m_pAction->actionType == BG::Match::Action::TAKES )
    {
    	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #4");
        return NotValid;
    }
    else if ( m_pAction->actionType == BG::Match::Action::DROPS )
    {
    	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #5");
        return Loose;
    }
    else if ( m_pAction->actionType == BG::Match::Action::RESIGNS )
    {
    	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #6");
        return NotValid;
    }
    else if ( m_pAction->actionType == BG::Match::Action::REJECTS )
    {
    	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #7");
        return NotValid;
    }
    else if ( m_pAction->actionType == BG::Match::Action::WINS )
    {
    	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #8");
        return Win;
    }
    else if ( m_pAction->actionType == BG::Match::Action::CRAWFORD )
    {
    	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #9");
        return NotValid;
    }

	syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl #10");
    NextActions();
    syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl after NextActions()");

    InitDice(_pvecbtPos);
    syslog( LOG_INFO | LOG_LOCAL0, "CGammonLogic::StepAnl after InitDice");

    return Valid;
}

void CGammonLogic::NextActions()
{

    m_pAction = m_pMatch->next();

    while(m_pAction->actionType != BG::Match::Action::ROLLS)
    {
        if ( m_pAction->actionType == BG::Match::Action::CRAWFORD )
        {
            m_pAction = m_pMatch->next();
        }
        else  if ( m_pAction->actionType == BG::Match::Action::MOVES )
        {
            m_pAction = m_pMatch->next();
        }
    }
 
}

void CGammonLogic::StartNewGame( )
{
    m_pMatch = TAPMatch( new BG::Match(1) );

    NextActions();
}


/*!
    \fn CGammonLogic::SetPos( const TVByte& _vecbtPos )
 */
bool CGammonLogic::SetPos( const TVByte& _vecbtPos )
{
    /// @todo debug me
    m_stream.clear();

    for(TVByte::const_iterator it = _vecbtPos.begin(); it != _vecbtPos.end(); ++it)
    {
#ifdef GMS_DEBUG
        std::cerr << *it;
        if (*it == '\r')
            std::cerr << "!r!";
        else if (*it == '\n')
            std::cerr << "!n!";

#endif
        m_stream << *it;
    }

    m_pMatch = TAPMatch( new BG::Match(m_stream) );

     m_pAction = &m_pMatch->last();

    return true;
}


/*!
    \fn CGammonLogic::GetPos()
 */
const TVByte*  CGammonLogic::GetPosForDB()
{
    /// @todo debug me
//    TVByte::value_type data;

    m_vecPos.clear();

    std::stringstream stream;

    stream.clear();

    std::string str_tmp = stream.str();

    m_pMatch->save(stream);

    std::string str = stream.str();

    m_vecPos.assign(str.begin(), str.end());

    /*while( !m_stream.eof() )
    {
        m_stream >> data;
        if (m_stream.good())
            m_vecPos.push_back(data);
    }*/

    return &m_vecPos;
}

const TVByte*  CGammonLogic::GetPosForClient()
{
 
    InitDataForClient();
    
    return &m_vecPosForClient;
}

void CGammonLogic::InitDataForClient()
{

    const BG::Board& brd = m_pMatch->board();
    //field
    for(int i = 0; i < BAR_X; ++i )
    {
         m_vecPosForClient.at(i) = brd.board[0][i];
         if (brd.board[1][23 - i]) 
             m_vecPosForClient.at(i) = -brd.board[1][23 - i];
    }

    //Bar
    m_vecPosForClient.at(BAR_X) = brd.board[0][24];
    m_vecPosForClient.at(BAR_O) = brd.board[1][24];

    m_vecPosForClient.at(DICE0) = m_pAction->dice[0];
    m_vecPosForClient.at(DICE1) = m_pAction->dice[1];

    m_vecPosForClient.at(STEP) = m_pAction->side;
}

void CGammonLogic::InitDice( TVByte* _pvecbtPos )
{
    _pvecbtPos->resize(3);
    _pvecbtPos->at(0) = m_pAction->dice[0];
    _pvecbtPos->at(1) = m_pAction->dice[1];
    _pvecbtPos->at(2) = m_pAction->side;
}

bool CGammonLogic::CheckSide(int _side)
{
    return m_pAction->side == _side;
}

