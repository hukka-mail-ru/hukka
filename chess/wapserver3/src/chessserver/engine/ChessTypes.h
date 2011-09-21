// $Id: ChessTypes.h,v 1.3 2003/12/06 16:44:51 psy Exp $
//
// ChessTypes.h
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

#if !defined(AFX_CHESSTYPES_H__EBEFC225_5256_11D3_BC7B_0080C84DB68D__INCLUDED_)
#define AFX_CHESSTYPES_H__EBEFC225_5256_11D3_BC7B_0080C84DB68D__INCLUDED_

#include "stdafx.h"
#include "IChessEngine.h"

enum piece_type {
    Empty=0,
        Pawn=1, Knight=2, Bishop=4, Rook=8, Queen=16, King=32,
    White=0,
        w_Pawn=Pawn+White, w_Knight=Knight+White, w_Bishop=Bishop+White,
        w_Rook=Rook+White, w_Queen=Queen+White,   w_King=King+White,
    Black=64,
        b_Pawn=Pawn+Black, b_Knight=Knight+Black, b_Bishop=Bishop+Black,
        b_Rook=Rook+Black, b_Queen=Queen+Black,   b_King=King+Black
};

enum square_type {
    a1, b1, c1, d1, e1, f1, g1, h1,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a8, b8, c8, d8, e8, f8, g8, h8,
    Rank=8, File=1, No_square=100
};

#pragma pack(1)

struct SPosition
{
    piece_type square[64];          // array of board squares
    bool w_turn;                    // flag for white to move
    bool w_castle_k;                // white can castle kingside
    bool w_castle_q;                // white can castle queenside
    bool b_castle_k;                // black can castle kingside
    bool b_castle_q;                // black can castle queenside
    bool b_castled;                 // black castled last move
    bool w_castled;                 // white castled last move
    bool w_check;                   // white is in check
    bool b_check;                   // black is in check
    int fifty;                      // fifty-move rule counter
    int en_passant;                 // location of en passant square
    int b_king_pos;                 // position of black king
    int w_king_pos;                 // position of white king
    ChessGameStatus status;             // status of game (checkmate, etc.)
    int last_move_from;
    int last_move_to;
};
#pragma pack()

#endif // !defined(AFX_CHESSTYPES_H__EBEFC225_5256_11D3_BC7B_0080C84DB68D__INCLUDED_)
