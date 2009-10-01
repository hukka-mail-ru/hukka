#include "CHEOPSEngine.h"

#include <iostream>


CHEOPSEngine::CHEOPSEngine()
{
}

CHEOPSEngine::CHEOPSEngine( String _strGameName )
{
    m_strGameName = _strGameName;
    
}

CHEOPSEngine::~CHEOPSEngine()
{
}

const TVByte* CHEOPSEngine::getPosition()
{
    m_vecbtPosition.clear();
    SPosition *position = m_Board.getPosition();
    
    for ( int i = 0; i < 64; ++i )
    {
        uint8_t pieceType = static_cast<uint8_t> ( position->square[i] );
        m_vecbtPosition.push_back( pieceType );
    }
    
    uint8_t w_turn = position->w_turn ? 1 : 0;
    uint8_t w_castle_k = position->w_castle_k ? 1 : 0;
    uint8_t w_castle_q = position->w_castle_q ? 1 : 0;
    uint8_t b_castle_k = position->b_castle_k ? 1 : 0;
    uint8_t b_castle_q = position->b_castle_q ? 1 : 0;
    uint8_t b_castled = position->b_castled ? 1 : 0;
    uint8_t w_castled = position->w_castled ? 1 : 0;
    uint8_t w_check = position->w_check ? 1 : 0;
    uint8_t b_check = position->b_check ? 1 : 0;
    
    m_vecbtPosition.push_back( w_turn );
    m_vecbtPosition.push_back( w_castle_k );
    m_vecbtPosition.push_back( w_castle_q );
    m_vecbtPosition.push_back( b_castle_k );
    m_vecbtPosition.push_back( b_castle_q );
    m_vecbtPosition.push_back( b_castled );
    m_vecbtPosition.push_back( w_castled );
    m_vecbtPosition.push_back( w_check );
    m_vecbtPosition.push_back( b_check );
    
    m_vecbtPosition.push_back( static_cast<uint8_t>( position->fifty) );        
    m_vecbtPosition.push_back( static_cast<uint8_t>( position->en_passant) );
    m_vecbtPosition.push_back( static_cast<uint8_t>( position->b_king_pos) );
    m_vecbtPosition.push_back( static_cast<uint8_t>( position->w_king_pos) );
    m_vecbtPosition.push_back( static_cast<uint8_t>( position->status) );    
    
    return &m_vecbtPosition;
}

uint32_t CHEOPSEngine::getPosSize()
{
    return sizeof( SPosition );
}


ChessGameStatus CHEOPSEngine::getResult()
{
    return m_Status;
    
}

bool CHEOPSEngine::isWhiteStep()
{
    return m_Board.white_moves();
}

bool CHEOPSEngine::move( const String& _strMove )
{
    SMove sMove;
    
    if ( ! m_Board.in_progress )
        return false;
    
    if (! parseMove( _strMove, &sMove ) )
        return false;
    
    if ( ! m_Board.can_move( sMove ) )
        return false;
    
    m_Status = m_Board.do_move( sMove );
    
    return true;
    
    
}

bool CHEOPSEngine::parseMove( const String& _strMove, SMove* _pMove )
{
    // Is this a valid move string?
    int len=_strMove.length();
    if (len>5 || len<4 || _strMove[0]<'a' || _strMove[0]>'h' || _strMove[2]<'a' ||
            _strMove[2]>'h' || _strMove[1]<'1' || _strMove[1]>'8' || _strMove[3]<'1' || _strMove[3]>'8')
        return false;
    if (len==5 && !strchr("qrnb", _strMove[4]))
        return false;

    // Determine promotion type

    if (len==5) {
        switch(_strMove[4]) {
        case 'q':
            _pMove->promotion=Queen;
            break;
        case 'r':
            _pMove->promotion=Rook;
            break;
        case 'n':
            _pMove->promotion=Knight;
            break;
        case 'b':
            _pMove->promotion=Bishop;
            break;
        }
    }
    else 
        _pMove->promotion=Empty;

    // Determine source and destination squares
    _pMove->from=_strMove[0]-'a'+(_strMove[1]-'1')*8;
    _pMove->to=_strMove[2]-'a'+(_strMove[3]-'1')*8;
    
    
    return true;

    
}

bool CHEOPSEngine::setPosition( const TVByte *_pPosition )
{
    if ( _pPosition->size() < 78 )
    {
        return false;
    }
    SPosition position;
    TVByte::const_iterator i = _pPosition->begin();
    
    for ( int n = 0; n < 64; ++n )
    {
        position.square[n] = static_cast<piece_type>(*i);
        ++i;
    }
    
    position.w_turn = *i++;
    position.w_castle_k = *i++;
    position.w_castle_q = *i++;
    position.b_castle_k = *i++;
    position.b_castle_q = *i++;
    position.b_castled = *i++;
    position.w_castled = *i++;
    position.w_check = *i++;
    position.b_check = *i++;
    position.fifty = *i++;
    position.en_passant = *i++;
    position.b_king_pos = *i++;
    position.w_king_pos = *i++;
    position.status = static_cast<ChessGameStatus>(*i);
    
    
    m_Board.setPosition( &position );
    
    return true;
}
