// $Id: ChessBoard.cpp,v 1.3 2003/12/06 16:44:51 psy Exp $
//
// ChessBoard.cpp: implementation of the ChessBoard class.
//
// Copyright (C) 2003 Tristan Miller <psychonaut@nothingisreal.com>
//
// This program is free software; you can redistribute it and/or
// modify it under the terms of the GNU General Public License as
// published by the Free Software Foundation; either version 2 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
// 02111-1307, USA.
//
//////////////////////////////////////////////////////////////////////

#include "stdafx.h"
#include "ChessBoard.h"
#include <stdlib.h>

//////////////////////////////////////////////////////////////////////
// Global functions
//////////////////////////////////////////////////////////////////////

// -------------------------------------------------------------------
// Print chess board (<< operator overload)
// -------------------------------------------------------------------
ostream &operator<<(ostream &os, const ChessBoard &c) {
    c.print(&os);
    return os;
}

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

bool ChessBoard::show_coordinates=true;
bool ChessBoard::show_reverse=false;

// -------------------------------------------------------------------
// Sets up a chess board in initial configuration, with white to move
// -------------------------------------------------------------------
ChessBoard::ChessBoard() {
    in_progress=true;
    m_CurPos.w_turn=true;
    m_CurPos.w_castle_k=true;
    m_CurPos.w_castle_q=true;
    m_CurPos.b_castle_k=true;
    m_CurPos.b_castle_q=true;
    m_CurPos.b_castled=false;
    m_CurPos.w_castled=false;
    m_CurPos.w_check=false;
    m_CurPos.b_check=false;
    m_CurPos.fifty=0;
    m_CurPos.en_passant=No_square;
    m_CurPos.w_king_pos=e1;
    m_CurPos.b_king_pos=e8;

    // Put the empty squares on the board
    int i;
    for (i=a3;i<a7;i++)
        m_CurPos.square[i]=Empty;

    // Put the pawns on the board
    for (i=a2;i<a3;i++) {
        m_CurPos.square[i]=w_Pawn;
        m_CurPos.square[i+5*Rank]=b_Pawn;
    }

    // Put the white pieces on the board
    m_CurPos.square[a1]=m_CurPos.square[h1]=w_Rook;
    m_CurPos.square[b1]=m_CurPos.square[g1]=w_Knight;
    m_CurPos.square[c1]=m_CurPos.square[f1]=w_Bishop;
    m_CurPos.square[d1]=w_Queen;
    m_CurPos.square[e1]=w_King;

    // Put the black pieces on the board
    m_CurPos.square[a8]=m_CurPos.square[h8]=b_Rook;
    m_CurPos.square[b8]=m_CurPos.square[g8]=b_Knight;
    m_CurPos.square[c8]=m_CurPos.square[f8]=b_Bishop;
    m_CurPos.square[d8]=b_Queen;
    m_CurPos.square[e8]=b_King;
}


ChessBoard::~ChessBoard() {
}

//////////////////////////////////////////////////////////////////////
// Other functions
//////////////////////////////////////////////////////////////////////

// Various short self-explanatory functions for performing menial status checks

bool ChessBoard::white_moves(void) const {
    return m_CurPos.w_turn;
}
bool ChessBoard::is_in_check(bool white) const {
    return white?m_CurPos.w_check:m_CurPos.b_check;
}

// -------------------------------------------------------------------
// Print chess board
// -------------------------------------------------------------------
void ChessBoard::print(ostream *os) const {
    int r_start = show_reverse ?  0:  7,
        r_end   = show_reverse ?  8: -1,
        r_inc   = show_reverse ?  1: -1,
        c_start = show_reverse ?  7:  0,
        c_end   = show_reverse ? -1:  8,
        c_inc   = show_reverse ? -1:  1;

    *os << endl;
    for (int row=r_start; row!=r_end; row+=r_inc) {
        *os << '\t';
        if (show_coordinates)
            *os << row+1 << "  ";
        for (int col=c_start; col!=c_end; col+=c_inc)
            switch (m_CurPos.square[row*Rank+col]) {
            case Empty:     *os << "- ";    break;
            case w_Pawn:    *os << "p ";    break;
            case b_Pawn:    *os << "P ";    break;
            case w_Knight:  *os << "n ";    break;
            case b_Knight:  *os << "N ";    break;
            case w_Bishop:  *os << "b ";    break;
            case b_Bishop:  *os << "B ";    break;
            case w_Rook:    *os << "r ";    break;
            case b_Rook:    *os << "R ";    break;
            case w_Queen:   *os << "q ";    break;
            case b_Queen:   *os << "Q ";    break;
            case w_King:    *os << "k ";    break;
            case b_King:    *os << "K ";    break;
            }
        *os << endl;
    }
    if (show_coordinates) {
        *os << endl << "\t   ";
        for (int col=c_start; col!=c_end; col+=c_inc)
            *os << (char)('a'+col) << ' ';
        *os << endl;
    }
}

// -------------------------------------------------------------------
// Piece movement:
//  - this function must be passed a *VALID* move (e.g. one that has
//    been verified by can_move())
//  - promotion piece will be automatically cast to correct colour
// -------------------------------------------------------------------
ChessGameStatus ChessBoard::do_move(int from, int to, piece_type promotion) {
    // Disable castling if rook is captured or moved
    for (int *sq=&to;sq;sq=sq==&to?&from:(int *)NULL)
        switch (*sq) {
        case a1:
            m_CurPos.w_castle_q=false;
            break;
        case a8:
            m_CurPos.b_castle_q=false;
            break;
        case h1:
            m_CurPos.w_castle_k=false;
            break;
        case h8:
            m_CurPos.b_castle_k=false;
        }

    // Reset castled flags
    m_CurPos.b_castled=m_CurPos.w_castled=false;

    // Mark en-passant square, if any
    if (m_CurPos.square[from]==w_Pawn && to==from+2*Rank)
        m_CurPos.en_passant=from+Rank;
    else if (m_CurPos.square[from]==b_Pawn && to==from-2*Rank)
        m_CurPos.en_passant=from-Rank;
    else
        m_CurPos.en_passant=No_square;

    // Perform actual rearrangement of board pieces
    bool increment_fifty=true;
    if (m_CurPos.w_castle_k && from==e1 && to==g1) {
        m_CurPos.square[g1]=w_King;                      // White castles kingside
        m_CurPos.square[f1]=w_Rook;
        m_CurPos.square[e1]=m_CurPos.square[h1]=Empty;
        m_CurPos.w_castled=true;
    }
    else if (m_CurPos.w_castle_q && from==e1 && to==c1) {
        m_CurPos.square[c1]=w_King;                      // White castles queenside
        m_CurPos.square[d1]=w_Rook;
        m_CurPos.square[e1]=m_CurPos.square[a1]=Empty;
        m_CurPos.w_castled=true;
    }
    else if (m_CurPos.b_castle_k && from==e8 && to==g8) {
        m_CurPos.square[g8]=b_King;                      // Black castles kingside
        m_CurPos.square[f8]=b_Rook;
        m_CurPos.square[e8]=m_CurPos.square[h8]=Empty;
        m_CurPos.b_castled=true;
    }
    else if (m_CurPos.b_castle_q && from==e8 && to==c8) {
        m_CurPos.square[c8]=b_King;                      // Black castles queenside
        m_CurPos.square[d8]=b_Rook;
        m_CurPos.square[e8]=m_CurPos.square[a8]=Empty;
        m_CurPos.b_castled=true;
    }
    else if (m_CurPos.square[from]==w_Pawn && m_CurPos.square[to]==Empty && to!=from+Rank) {
        m_CurPos.square[to]=w_Pawn;                      // White captures en-passant
        m_CurPos.square[from]=m_CurPos.square[to-Rank]=Empty;
        increment_fifty=false;
    }
    else if (m_CurPos.square[from]==b_Pawn && m_CurPos.square[to]==Empty && to!=from-Rank) {
        m_CurPos.square[to]=b_Pawn;                      // Black captures en-passant
        m_CurPos.square[from]=m_CurPos.square[to+Rank]=Empty;
        increment_fifty=false;
    }
    else {                                      // Normal move
        if (m_CurPos.square[to]!=Empty || is_pawn(m_CurPos.square[from]))
            increment_fifty=false;
        promotion=make_colour(promotion, m_CurPos.w_turn);
        m_CurPos.square[to]=(promotion!=Empty)?promotion:m_CurPos.square[from];
        m_CurPos.square[from]=Empty;
    }

    // Disable castling if king was moved
    if (m_CurPos.square[e1]!=w_King)
        m_CurPos.w_castle_q=m_CurPos.w_castle_k=false;
    if (m_CurPos.square[e8]!=b_King)
        m_CurPos.b_castle_q=m_CurPos.b_castle_k=false;

    // Update king position
    if (m_CurPos.square[to]==w_King)
        m_CurPos.w_king_pos=to;
    else if (m_CurPos.square[to]==b_King)
        m_CurPos.b_king_pos=to;

    // Adjust 50-move rule counter
    m_CurPos.fifty=increment_fifty?m_CurPos.fifty+1:0;

    // Set check flag
    if (m_CurPos.w_turn) {
        m_CurPos.w_check=is_attacked(!m_CurPos.w_turn, m_CurPos.w_king_pos);
        m_CurPos.b_check=is_attacked(m_CurPos.w_turn, m_CurPos.b_king_pos);
    }
    else {
        m_CurPos.b_check=is_attacked(!m_CurPos.w_turn, m_CurPos.b_king_pos);
        m_CurPos.w_check=is_attacked(m_CurPos.w_turn, m_CurPos.w_king_pos);
    }

    // Change active player
    m_CurPos.w_turn=!m_CurPos.w_turn;

    // Check for checkmate/stalemate
    move_list moves;
    for (int i=a1;i<=h8;i++) {
        if (is_colour(m_CurPos.square[i], m_CurPos.w_turn)) {
            switch (make_neutral(m_CurPos.square[i])) {
            case King:
                moves=mobility_king(i);
                break;
            case Pawn:
                moves=mobility_pawn(i);
                break;
            case Knight:
                moves=mobility_knight(i);
                break;
            case Bishop:
                moves=mobility_bishop(i);
                break;
            case Rook:
                moves=mobility_rook(i);
                break;
            case Queen:
                moves=mobility_queen(i);
                break;
            }
        if (!moves.empty())
            break;
        }
    }

    // Update log of board configurations
    boardlog.append(*this);

    // Set game status
    if (triple_occurrence()) {
        m_CurPos.status=TripleOccurrence;
        in_progress=false;
    }
    if (no_pieces_for_mate()) {
        m_CurPos.status=Stalemate;
        in_progress=false;
    }
    else if (moves.empty() && is_in_check(m_CurPos.w_turn)) {
        m_CurPos.status=Checkmate;
        in_progress=false;
    }
    else if (moves.empty()) {
        m_CurPos.status=Stalemate;
        in_progress=false;
    }
    else if (m_CurPos.fifty>=99) {
        m_CurPos.status=FiftyMoves;
        in_progress=false;
    }
    else if (is_in_check(m_CurPos.w_turn))
        m_CurPos.status=Check;
    else
        m_CurPos.status=Normal;

    return m_CurPos.status;
}

// -------------------------------------------------------------------
// King against King = a draw.
// -------------------------------------------------------------------
bool ChessBoard::no_pieces_for_mate(void) const {
    int num = 0;
    for (int i=a1;i<=h8;i++) {
        piece_type piece = make_neutral(m_CurPos.square[i]);
        if (piece == King || piece == Empty) {
            continue;
        }
        else
        {
            num++;
        }
    }
    return num == 0;
}

// -------------------------------------------------------------------
// Determine whether we have just encountered triple occurrence of the
// same board configuration
// -------------------------------------------------------------------
bool ChessBoard::triple_occurrence(void) const {
    static string thisboard;
    static int i, pos;
    thisboard=*this;
    for (i=pos=0;i<3;i++)
        if ((pos=boardlog.find(thisboard, pos))==-1)
            return false;
        else
            pos++;
    return true;
}

// -------------------------------------------------------------------
// Convert a ChessBoard to a string
// -------------------------------------------------------------------
ChessBoard::operator string() const {
    static char s[66];
    s[0]=m_CurPos.w_turn?'W':'L';
    for (int i=0;i<64;i++)
        switch(m_CurPos.square[i]) {
            case Empty:     s[i+1]='-'; break;
            case w_Pawn:    s[i+1]='p'; break;
            case b_Pawn:    s[i+1]='P'; break;
            case w_Knight:  s[i+1]='n'; break;
            case b_Knight:  s[i+1]='N'; break;
            case w_Bishop:  s[i+1]='b'; break;
            case b_Bishop:  s[i+1]='B'; break;
            case w_Rook:    s[i+1]='r'; break;
            case b_Rook:    s[i+1]='R'; break;
            case w_Queen:   s[i+1]='q'; break;
            case b_Queen:   s[i+1]='Q'; break;
            case w_King:    s[i+1]='k'; break;
            case b_King:    s[i+1]='K'; break;
        }
    s[65]='\0';
    return (string)s;
}

// -------------------------------------------------------------------
// Determine whether the given move is legal
//  - from and to must be in [a1..h8]
//  - promotion piece will be automatically cast to correct colour
// -------------------------------------------------------------------
bool ChessBoard::can_move(int from, int to, piece_type promotion) const {

    // Reject if game is not in progress
    if (!in_progress)
        return false;

    // Reject if that square is not occupied by the player's piece
    if (m_CurPos.w_turn && !is_white(m_CurPos.square[from]) || !m_CurPos.w_turn && !is_black(m_CurPos.square[from]))
        return false;

    move_list moves;
    switch (make_neutral(m_CurPos.square[from])) {
    case King:
        moves=mobility_king(from);
        break;
    case Pawn:
        moves=mobility_pawn(from);
        break;
    case Knight:
        moves=mobility_knight(from);
        break;
    case Bishop:
        moves=mobility_bishop(from);
        break;
    case Rook:
        moves=mobility_rook(from);
        break;
    case Queen:
        moves=mobility_queen(from);
        break;
    }

    for (move_list::iterator i=moves.begin(); i!=moves.end(); ++i)
        if ((*i).to==to && (*i).promotion==promotion)
        {
            cerr << "ChessBoard::can_move true" << endl;
            return true;
        }

    cerr << "ChessBoard::can_move false" << endl;
    return false;
}

// -------------------------------------------------------------------
// Determine king mobility
//  - returns a list of all squares where the king may move
// -------------------------------------------------------------------
move_list ChessBoard::mobility_king(int from) const {
    SMove m;
    move_list ml;
    m.promotion=Empty;
    m.from=from;

    // Potential destinations relative to king's position
    static const int relative_destination[] = {-Rank-File, -Rank, -Rank+File,
        -File, File, Rank-File, Rank, Rank+File};

    for (int i=0;i<8;i++) {
        m.to=from+relative_destination[i];
        if (is_dest(m.from,m.to) && abs(which_file(from)-which_file(m.to))<=1 &&
            !causes_check(m))
            ml.push_front(m);
    }

    // Can king castle kingside?
    register bool white=is_white(m_CurPos.square[from]);
    if (// 1) Castling flag is set (i.e. king/rook not moved/captured)
        (white ? m_CurPos.w_castle_k : m_CurPos.b_castle_k) == true  &&
        // 2) King is not castling to escape check
        (white ? m_CurPos.w_check    : m_CurPos.b_check)    == false &&
        // 3) King is not castling through check
        is_attacked(!white, from+File)    == false &&
        // 4) King is not castling into check
        is_attacked(!white, from+2*File)  == false &&
        // 5) Squares between king and rook are empty
        m_CurPos.square[from+File]==Empty && m_CurPos.square[from+2*File]==Empty ) {
            m.to=from+2*File;
            ml.push_front(m);
    }

    // Can king castle queenside?
    if (// 1) Castling flag is set (i.e. king/rook not moved/captured)
        (white ? m_CurPos.w_castle_q : m_CurPos.b_castle_q) == true  &&
        // 2) King is not castling to escape check
        (white ? m_CurPos.w_check    : m_CurPos.b_check)    == false &&
        // 3) King is not castling through check
        is_attacked(!white, from-File)    == false &&
        // 4) King is not castling into check
        is_attacked(!white, from-2*File)  == false &&
        // 5) Squares between king and rook are empty
        m_CurPos.square[from-File]==Empty && m_CurPos.square[from-2*File]==Empty &&
        m_CurPos.square[from-3*File]==Empty) {
            m.to=from-2*File;
            ml.push_front(m);
    }

    return ml;
}

// -------------------------------------------------------------------
// Determine knight mobility
//  - returns a list of all squares where the knight may move
// -------------------------------------------------------------------
move_list ChessBoard::mobility_knight(int from) const {
    SMove m;
    move_list ml;
    m.promotion=Empty;
    m.from=from;

    // Potential destinations relative to knight's position
    static const int relative_destination[] = {-2*Rank-File, -2*Rank+File,
        -2*File-Rank, -2*File+Rank, 2*Rank-File, 2*Rank+File, 2*File-Rank,
        2*File+Rank};

    for (int i=0;i<8;i++) {
        m.to=from+relative_destination[i];
        if (is_dest(m.from,m.to) && abs(which_file(from)-which_file(m.to))<3 &&
            !causes_check(m))
            ml.push_front(m);
    }

    return ml;
}

// -------------------------------------------------------------------
// Determine pawn mobility
//  - returns a list of all squares where the pawn may move
// -------------------------------------------------------------------
move_list ChessBoard::mobility_pawn(int from) const {
    SMove m;
    move_list ml;
    m.promotion=Empty;
    m.from=from;
    const int ahead=is_white(m_CurPos.square[from])?1:-1;
    const int capture_destination[]=
        {m.from+ahead*Rank+File, m.from+ahead*Rank-File};
    int i;

    // Check for captures (normal or en-passant)
    for (i=0;i<2;i++) {
        m.to=capture_destination[i];
        if (abs(which_file(m.from)-which_file(m.to))==1 &&
            (is_white(m_CurPos.square[m.from]) && is_black(m_CurPos.square[m.to]) ||
             is_black(m_CurPos.square[m.from]) && is_white(m_CurPos.square[m.to]) ||
             m_CurPos.en_passant==m.to) && !causes_check(m)) {
            if (is_edge(m.to))
            {
                cerr << "ChessBoard::mobility_pawn 1" << endl;

                m.promotion=Bishop; ml.push_front(m);
                m.promotion=Rook;   ml.push_front(m);
                m.promotion=Knight; ml.push_front(m);
                m.promotion=Queen;  ml.push_front(m);
                m.promotion=Empty;
            }
            else
            {
                cerr << "ChessBoard::mobility_pawn 2" << endl;
                ml.push_front(m);
            }
        }
    }

    // Check two squares ahead
    m.to=m.from+ahead*2*Rank;
    if (which_rank(from)==1 && is_white(m_CurPos.square[from]) &&
            m_CurPos.square[m.to]==Empty && m_CurPos.square[m.from+ahead*Rank]==Empty &&
        !causes_check(m))
    {
        cerr << "ChessBoard::mobility_pawn 3" << endl;
        ml.push_front(m);
    }
    else if (which_rank(from)==6 && is_black(m_CurPos.square[from]) &&
            m_CurPos.square[m.to]==Empty && m_CurPos.square[m.from+ahead*Rank]==Empty &&
        !causes_check(m))
    {
        cerr << "ChessBoard::mobility_pawn 4" << endl;
        ml.push_front(m);
    }

    // Check one square ahead
    m.to=m.from+ahead*Rank;
    if (!is_edge(m.to) && m_CurPos.square[m.to]==Empty && !causes_check(m))
    {
        ml.push_front(m);
        cerr << "ChessBoard::mobility_pawn 5" << endl;
    }
    else if (is_edge(m.to) && m_CurPos.square[m.to]==Empty && !causes_check(m))
    {
        m.promotion=Bishop; ml.push_front(m);
        m.promotion=Rook;   ml.push_front(m);
        m.promotion=Knight; ml.push_front(m);
        m.promotion=Queen;  ml.push_front(m);
        cerr << "ChessBoard::mobility_pawn 6" << endl;
    }

    return ml;
}

// -------------------------------------------------------------------
// Determine rook mobility
//  - returns a list of all squares where the rook may move
// -------------------------------------------------------------------
move_list ChessBoard::mobility_rook(int from) const {
    SMove m;
    move_list ml;
    m.promotion=Empty;
    m.from=from;
    int i;
    static const int increment[]={-Rank, Rank, -File, File};

    for(i=0;i<4;i++) {
        for (m.to=from+increment[i];
             abs(which_file(m.to)-which_file(m.to-increment[i]))<=1;
             m.to+=increment[i]) {
            if (!is_dest(m.from, m.to))
                break;
            if (!causes_check(m))
                ml.push_front(m);
            if (m_CurPos.square[m.to]!=Empty)
                break;
        }
    }

    return ml;
}

// -------------------------------------------------------------------
// Determine bishop mobility
//  - returns a list of all squares where the bishop may move
// -------------------------------------------------------------------
move_list ChessBoard::mobility_bishop(int from) const {
    SMove m;
    move_list ml;
    m.promotion=Empty;
    m.from=from;
    int i;
    static const int increment[]={-Rank-File, Rank-File, -Rank+File, Rank+File};

    for(i=0;i<4;i++) {
        for (m.to=from+increment[i];
             abs(which_file(m.to)-which_file(m.to-increment[i]))==1;
             m.to+=increment[i]) {
            if (!is_dest(m.from, m.to))
                break;
            if (!causes_check(m))
                ml.push_front(m);
            if (m_CurPos.square[m.to]!=Empty)
                break;
        }
    }

    return ml;
}

// -------------------------------------------------------------------
// Determine queen mobility
//  - returns a list of all squares where the queen may move
// -------------------------------------------------------------------
move_list ChessBoard::mobility_queen(int from) const {
    move_list rl, bl;
    rl=mobility_rook(from);
    bl=mobility_bishop(from);
    rl.splice(rl.begin(),bl);
    return rl;
}

// -------------------------------------------------------------------
// Determine whether the given square is under attack by a given side
// -------------------------------------------------------------------
bool ChessBoard::is_attacked(bool by_white, int to) const {
    int from, i;
    const int ahead=by_white?-1:1;

    // Check for pawn attacks
    from=to+ahead*Rank-File;
    if (is_on_board(from) && m_CurPos.square[from]==make_colour(Pawn, by_white) &&
        abs(which_file(from)-which_file(to))==1)
        return true;
    from=to+ahead*Rank+File;
    if (is_on_board(from) && m_CurPos.square[from]==make_colour(Pawn, by_white) &&
        abs(which_file(from)-which_file(to))==1)
        return true;

    // Check for bishop/queen attacks
    static const int b_increment[]={-Rank-File, Rank-File, -Rank+File,
        Rank+File};
    for(i=0;i<4;i++) {
        for (from=to+b_increment[i]; is_on_board(from) &&
            abs(which_file(from)-which_file(from-b_increment[i]))==1;
            from+=b_increment[i]) {
            if (m_CurPos.square[from]==make_colour(Bishop, by_white) ||
                    m_CurPos.square[from]==make_colour(Queen, by_white))
                return true;
            if (m_CurPos.square[from]!=Empty)
                break;
        }
    }

    // Check for rook/queen attacks
    static const int r_increment[]={-Rank, Rank, -File, File};
    for(i=0;i<4;i++) {
        for (from=to+r_increment[i]; is_on_board(from) &&
            abs(which_file(from)-which_file(from-r_increment[i]))<=1;
            from+=r_increment[i]) {
            if (m_CurPos.square[from]==make_colour(Rook, by_white) ||
                    m_CurPos.square[from]==make_colour(Queen, by_white))
                return true;
            if (m_CurPos.square[from]!=Empty)
                break;
        }
    }

    // Check for knight attacks
    static const int n_source[] = {-2*Rank-File, -2*Rank+File, -2*File-Rank,
        -2*File+Rank, 2*Rank-File, 2*Rank+File, 2*File-Rank, 2*File+Rank};
    for (i=0;i<8;i++) {
        from=to+n_source[i];
        if (is_on_board(from) && m_CurPos.square[from]==make_colour(Knight, by_white) &&
            abs(which_file(from)-which_file(to))<3)
            return true;
    }

    // Check for king attacks
    static const int k_source[] = {-Rank-File, -Rank, -Rank+File, -File,
        File, Rank-File, Rank, Rank+File};
    for (i=0;i<8;i++) {
        from=to+k_source[i];
        if (is_on_board(from) && m_CurPos.square[from]==make_colour(King, by_white) &&
            abs(which_file(from)-which_file(to))<=1)
            return true;
    }

    // Check for en passant attacks
    if (to-ahead*Rank==m_CurPos.en_passant &&
        (m_CurPos.square[to-File]==make_colour(Pawn, by_white) ||
                m_CurPos.square[to+File]==make_colour(Pawn, by_white)))
        return true;

    return false;
}

// -------------------------------------------------------------------
// Determine whether the given move puts one's own king in check
// -------------------------------------------------------------------
bool ChessBoard::causes_check(const SMove& m) const {
    bool w_turn=is_white(m_CurPos.square[m.from]);
    ChessBoard c=*this;
    piece_type promotion=m.promotion;

    if (c.m_CurPos.square[m.from]==w_Pawn && c.m_CurPos.square[m.to]==Empty &&
        m.to!=m.from+Rank) {
        c.m_CurPos.square[m.to]=w_Pawn;                      // White captures en-passant
        c.m_CurPos.square[m.from]=c.m_CurPos.square[m.to-Rank]=Empty;
    }
    else if (c.m_CurPos.square[m.from]==b_Pawn && c.m_CurPos.square[m.to]==Empty &&
        m.to!=m.from-Rank) {
        c.m_CurPos.square[m.to]=b_Pawn;                      // Black captures en-passant
        c.m_CurPos.square[m.from]=c.m_CurPos.square[m.to+Rank]=Empty;
    }
    else {
        promotion=make_colour(promotion, w_turn);   // Normal move
        c.m_CurPos.square[m.to]=(promotion!=Empty)?promotion:c.m_CurPos.square[m.from];
        c.m_CurPos.square[m.from]=Empty;
    }

    // Update king position
    if (c.m_CurPos.square[m.to]==w_King)
        c.m_CurPos.w_king_pos=m.to;
    else if (c.m_CurPos.square[m.to]==b_King)
        c.m_CurPos.b_king_pos=m.to;

    return w_turn?
        c.is_attacked(!m_CurPos.w_turn, c.m_CurPos.w_king_pos):
        c.is_attacked(!m_CurPos.w_turn, c.m_CurPos.b_king_pos);
}

void ChessBoard::setPosition( SPosition* _pCurPos )
{
    m_CurPos = *_pCurPos;
}

SPosition* ChessBoard::getPosition()
{
    return &m_CurPos;
}
