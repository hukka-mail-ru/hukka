#if defined( __GNUG__ )
#pragma implementation
#endif


/*
 * bgdefs.cc
 *
 * by Joseph Heled, 2002
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of version 2 of the GNU General Public License as
 * published by the Free Software Foundation.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 */

#include <algorithm>
#include <string.h>

#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include "bgdefs.h"

namespace BG {

#ifndef VERSION
const char* const version = "VERSION";
#else
const char* const version = VERSION;
#endif

const char* const applicationName = "BG light";

void
Board::swapSides(void)
{
  for(uint i = 0; i < sizeof(board[0])/sizeof(board[0][0]); ++i) {
    std::swap(board[0][i], board[1][i]);
  }
}

void
Board::initial(void)
{
  memset(board[0], 0, sizeof(board[0]));
  memset(board[1], 0, sizeof(board[0]));
  for(uint side = 0; side < 2; ++side) {
    board[side][5] = 5;
    board[side][7] = 3; 
    board[side][12] = 5; 
    board[side][23] = 2;
  }
}

Board::Board(Board const& b, bool const swap)
{
  memcpy(&board[swap ? 1 : 0][0], b[0], sizeof(board[0]));
  memcpy(&board[swap ? 0 : 1][0], b[1], sizeof(board[1]));
}

Board&
Board::operator =(Board const& b)
{
  memcpy(&board[0][0], b[0], sizeof(board[0]));
  memcpy(&board[1][0], b[1], sizeof(board[1]));

  return *this;
}

bool
Board::isRace(void) const
{
  int nOppBack;

  const uint* const b1 = board[1];
  
  for(nOppBack = 24; nOppBack >= 0; --nOppBack) {
    if( b1[nOppBack] ) {
      break;
    }
  }
    
  nOppBack = 23 - nOppBack;

  const uint* const b0 = board[0];
  
  for(int i = nOppBack + 1; i < 25; ++i) {
    if( b0[i] ) {
      return false;
    }
  }

  return true;
}

bool
operator ==(Board const& b1, Board const& b2)
{
  return memcmp(&b1, &b2, sizeof(b1)) == 0;
}
  

}
