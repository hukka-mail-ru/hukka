#if defined( __GNUG__ )
#pragma implementation
#endif

/*
 * moves.cc
 *
 * by Joseph Heled, 2002
 *
 * Eric Groleau move generator, adapted from eggrules.c to GNU BG. 
 * by Joseph Heled. Fixed some bugs as well 
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

#include <cassert>
#include <string.h>
#include <stdlib.h>
#include <algorithm>

#include "moves.h"

using namespace BG;

#ifdef WIN32
#include <windows.h>
#else
using std::min;
#endif


/*
  List of problems found and fixed,
  
  1. egg did not find the duplication here.
  
  0 -4 -3 -2 -1 0 -2 0 -2 0 0 0 0 0 0 0 0 0 0 0 1 1 3 6 2 -1 
  dice 5 6
  Gary:
  0)  4--2 3--2 {0 -4 -3 -2 -1 0 -2 0 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 3 6 2 -1}
  Egg:
  0)  5-0 4-0 {0 -4 -3 -2 -1 0 -2 0 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 3 6 2 -1}
  1)  5-0 4-0 {0 -4 -3 -2 -1 0 -2 0 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 3 6 2 -1}


  2. egg missed a move.

  0 0 1 -2 -2 -3 -1 0 0 0 0 0 0 -2 0 0 0 0 -1 0 0 -3 2 5 -1 0 
  dice 1 3
  Gary:
  0)  2-1 {0 0 1 -2 -2 -3 -1 0 0 0 0 0 0 -2 0 0 0 0 -1 0 0 -3 1 6 -1 0}
  1)  1-0 {0 0 1 -2 -2 -3 -1 0 0 0 0 0 0 -2 0 0 0 0 -1 0 0 -3 2 4 1 -1}
  Egg:
  0)  2-1 {0 0 1 -2 -2 -3 -1 0 0 0 0 0 0 -2 0 0 0 0 -1 0 0 -3 2 4 1 -1}

  3. egg missed 2 moves
  0 0 -1 -2 -2 -2 -2 0 -1 0 -2 0 0 0 0 0 0 2 0 3 -1 3 5 -2 2 0
  Dice 6 3

  Gary:
  0)  7-4 {0 0 -1 -2 -2 -2 -2 0 -1 0 -2 0 0 0 0 0 0 1 0 3 1 3 5 -2 2 -1}
  1)  5-2 {0 0 -1 -2 -2 -2 -2 0 -1 0 -2 0 0 0 0 0 0 2 0 2 -1 3 6 -2 2 0}
  2)  3-0 {0 0 -1 -2 -2 -2 -2 0 -1 0 -2 0 0 0 0 0 0 2 0 3 -1 2 5 -2 3 0}
  Egg:
  0)  8-5 {0 0 -1 -2 -2 -2 -2 0 -1 0 -2 0 0 0 0 0 0 1 0 3 1 3 5 -2 2 -1}

  3. BG rules says you have to use both dice is possible.

  0 -3 -3 -3 -2 0 -3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0 
  dice 1 2
  Gary:
  0)  1-0 0--2 {0 -3 -3 -3 -2 0 -3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1}
  Egg:
  0)  2-1 1-0 {0 -3 -3 -3 -2 0 -3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1}
  1)  2-0 {0 -3 -3 -3 -2 0 -3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 -1 0}

  4. used only one dice in move description
   0 -2 -2 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 0 0 0
   Dice 5 2
*/


// Number of points in board representation
namespace {

unsigned int const BAR = 25;

// in GNU, we only move player 1
unsigned int const turn = 1;

}

		       


bool
MoveGenerator::moveChecker(EggBoard    board,
			   uint const  dice,
			   uint const  point,
			   uint const  maxPoint,
			   Emove&      move,
			   int         d)
{
  // Verify if i have a checker on that point.

  if( !board[turn][point] )
    return 0;

  // Arrival point
  int arrivalPoint = point - dice;

  // Do we have a checker stuck on the BAR?

  if( (point != BAR) && board[turn][BAR] ) {
    return false;
  }

  // If we're bearing off, arrivalPoint = 0

  if( arrivalPoint <= 0 ) {
    // can't move from HOME point. rare but the algorithm may try to do that
    // say play 1,2 for
    // {0 -3 -3 -3 -2 0 -3 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 -1 0}

    if( point == 0 ) {
      return false;
    }
    
    if( (maxPoint <= 6) && ((point == maxPoint) || (point == dice)) ) {
      arrivalPoint = 0;
    } else {
      return false;
    }
  }
  
  // I can move if opponent has 0 or 1 checker on arrivalPoint 
  // or if arrivalPoint = 0 (bearoff)

  if( (board[1-turn][BAR-arrivalPoint] < 2) || (arrivalPoint == 0) ) {

    --d;
    
    // Record the move

    move.from[d] = point;
    move.to[d]   = arrivalPoint;

    // Change the board accordingly

    board[turn][point]--;
    board[turn][arrivalPoint]++;

    // If we hit a blot, move it to the bar

    if( (arrivalPoint != 0) && board[1-turn][BAR-arrivalPoint]) {
      board[1-turn][BAR-arrivalPoint]--;
      board[1-turn][BAR]++;
      move.to[d] = -move.to[d];
    }
    
    return true;     // The checker has been moved.
  }

  return false;     // The checker could not move.
}


void
MoveGenerator::unmoveChecker(EggBoard board, Emove const& move, int d)
{
  --d;
  
  int const departurePoint = move.from[d];
  int arrivalPoint = move.to[d];

  // If a blot was hit, put it back from the bar

  if( arrivalPoint < 0 ) {
    arrivalPoint = -arrivalPoint;
    
    board[1-turn][BAR-arrivalPoint]++;
    board[1-turn][BAR]--;
  }

  // Put our own checker back

  board[turn][departurePoint]++;
  board[turn][arrivalPoint]--;
}

int
MoveGenerator::movesNonDouble(EggBoard board, Emove* const moves, int d1, int d2)
{
  unsigned int nMoves = 0;

  for(uint i = 0; i < 4; i++) {
    moves[0].from[i] = 0;
    moves[0].to[i] = 0;
  }

  // highest occupied point
  int maxp1;
  for(maxp1 = BAR; !board[turn][maxp1]; maxp1--)
    ;

  // Search once, invert the dice order and search again

  for(uint seq = 0; seq < 2; seq++) {

    // If we have a checker past the 6 point, we can't bearoff yet

    int minp1 = (maxp1 > 6) ? d1 : min(d1, maxp1) - 1;

    // Search from maxp1 to the lowest legal point

    for(int p1 = maxp1; p1 > minp1; p1--) {

      // Skip empty points

      while( !board[turn][p1]) {
	--p1;
      }

      // See if we can move this checker with d1

      if( moveChecker(board, d1, p1, maxp1, moves[nMoves], 1)) {

        moves[nMoves].unplayedDice = 1;         // 1 checker left to move

        // Find the highest occupied point. Could have changed since d1

	int maxp2;
	for(maxp2 = maxp1; !board[turn][maxp2]; maxp2--);

        // If we have a checker past the 6 point, we can't bearoff yet

	int minp2 = (maxp2 > 6) ? d2 : min(d2, maxp2) - 1;

        for(int p2 = p1 - seq; p2 > minp2; p2--) {

	  // Skip empty points

	  while( !board[turn][p2] ) {
	    --p2;
	  }

	  // See if we can move the second checker with d2 */

          if( moveChecker(board, d2, p2, maxp2, moves[nMoves], 2)) {

            // The move is complete. Verify that it is not a replicate
            // With our searching algorith, the only possible          
            // replication is if the same checker is moved with both   
            // dice and no blot was taken with the first dice.         

            bool replicate = false;
	    
            if( (moves[nMoves].to[0] == moves[nMoves].from[1]) ) {
              for(uint i = 0; i < nMoves; ++i) {
		// don't compare incomplete move with this complete move. We 
		// want the complete move to 'override' an incomplete one.
		
                if( moves[i].unplayedDice == 0 &&
		    (moves[i].from[0] == moves[nMoves].from[0]) &&
		    (moves[i].to[0] == moves[i].from[1])) {
                  replicate = true;
		  break;
		}
	      }
	    }

	    // Another source for replication is when both dice where used  
	    // to bear off, say 5 6 in                                      
	    // {0 -4 -3 -2 -1 0 -2 0 -2 0 0 0 0 0 0 0 0 0 0 0 1 1 3 6 2 -1} 
	    
            if( !replicate &&
		(moves[nMoves].to[0] == 0 && moves[nMoves].to[1] == 0) ) {
              for(uint i = 0; i < nMoves; ++i)
                if( memcmp(moves[i].from, moves[nMoves].from,
			   2 * sizeof(moves[i].from[0])) == 0 &&
		    memcmp(moves[i].to, moves[nMoves].to,
			   2 * sizeof(moves[i].to[0])) == 0 ) {
                  replicate = true;
		  break;
		}
	    }

            unmoveChecker(board, moves[nMoves], 2);

            if( !replicate ) {

              // It's not a replicate. Keep it.

              moves[nMoves].unplayedDice = 0;    // move complete
              moves[nMoves].from[2] = moves[nMoves].from[3] = 0;
              moves[nMoves].to[2] = moves[nMoves].to[3] = 0;

              // Erase previously recorded less complete moves.

              if( moves[0].unplayedDice > moves[nMoves].unplayedDice ) {
		moves[0] = moves[nMoves];
                nMoves = 0;
              }

              ++nMoves;
              moves[nMoves].unplayedDice = moves[nMoves-1].unplayedDice + 1;
	      
	      moves[nMoves].from[0] = moves[nMoves-1].from[0];
	      moves[nMoves].to[0]   = moves[nMoves-1].to[0];
            }

          }
        }

        unmoveChecker(board, moves[nMoves], 1);

        // Record the move if no more complete move has been recorded

        if( moves[nMoves].unplayedDice <= moves[0].unplayedDice ) {

          if( (moves[nMoves].from[0] - abs(moves[nMoves].to[0])) >
              (moves[0].from[0] - abs(moves[0].to[0]))) {
	    
	    moves[0].from[0] = moves[nMoves].from[0];
	    moves[0].to[0] = moves[nMoves].to[0];

	    for(uint i = 1; i < 4; i++) {
              moves[0].from[i] = 0;
              moves[0].to[i] = 0;
	    }

            nMoves = 1;
          }

          if( (nMoves == 0) ||
              (((moves[nMoves].from[0] - abs(moves[nMoves].to[0])) ==
                (moves[0].from[0] - abs(moves[0].to[0]))) &&
               (moves[nMoves].from[0] != moves[0].from[0]))) {
            for(uint i = 1; i < 4; i++) {
              moves[nMoves].from[i] = 0;
              moves[nMoves].to[i] = 0;
	    }
            nMoves++;
          }
        }
      
      }
    }

    // Invert the dice to look for new moves.

    std::swap(d1, d2);
  }

  return nMoves;
}

void
MoveGenerator::docall(EggBoard eb, Emove const& m)
{
  MoveDesc move;
  uint nDesc = 1;
  for(/**/; nDesc < 4 && m.from[nDesc] != 0; ++nDesc)
    ;
  for(uint k = 0; k < nDesc; ++k) {
    int const j = 2 * k;
      
    move.desc[j] = m.from[k];
    move.desc[j+1] = abs(m.to[k]);
  }
  if( nDesc < 4 ) {
    move.desc[2*nDesc] = -1;
  }

  Board b;
  memcpy(&b.board[0], &eb[0][1], sizeof(b.board[0]));
  memcpy(&b.board[1], &eb[1][1], sizeof(b.board[1]));
      
  callback(b, move);
}

static inline int
next(int const board[2][26], int from)
{
  while( board[turn][from] == 0 ) {
    {                                                     assert( from > 0 ); }
    --from;
  }
  return from;
}

int
MoveGenerator::movesDouble(EggBoard board, int dice)
{
  // number of possible moves
  int nMoves = 0;

  Emove moves;
  for(uint i = 0; i < 4; ++i) {
    moves.from[i] = 0;
    moves.to[i] = 0;
  }

  
  int const maxp1 = next(board, BAR);
  
  // Start searching

  for(int p1 = maxp1; p1 > 0; p1--) {
    while (!board[turn][p1] && p1) p1--;
    
    if( moveChecker(board,  dice, p1, maxp1, moves, 1)) {

      moves.unplayedDice = 3;           // 3 checkers left to move

      int const maxp2 = next(board, maxp1);
      int const minp2 = (maxp2 > 6) ? dice : min(dice, maxp2) - 1;

      for(int p2 = p1; p2 > minp2; p2-- ) {
        while( !board[turn][p2] && p2 ) p2--;
	
        if( moveChecker(board, dice, p2, maxp2, moves,2)) {

          moves.unplayedDice = 2;    // 2 checkers left to move

	  int const maxp3 = next(board, maxp2);
	  int const minp3 = (maxp3 > 6) ? dice : min(dice, maxp3) - 1;

          for(int p3 = p2; p3 > minp3; p3--) {
            while( !board[turn][p3] && p3 ) {
	      --p3;
	    }
	    
            if( moveChecker(board, dice, p3, maxp3,  moves, 3) ) {

              moves.unplayedDice = 1;   // 1 checker left to move 

	      int const maxp4 = next(board, maxp3);
	      int const minp4 = (maxp4 > 6) ? dice : min(dice, maxp4) - 1;

              for(int p4 = p3; p4 > minp4; p4--) {
                while( !board[turn][p4] && p4 ) {
		  --p4;
		}
		
                if( moveChecker(board,  dice, p4, maxp4, moves, 4)) {

                  moves.unplayedDice = 0; // move complete 

		  docall(board, moves);
		    
                  unmoveChecker(board, moves, 4);

                  nMoves++;
                  moves.unplayedDice = moves.unplayedDice + 1;
                }
              }

              if( nMoves == 0 ) {
		// partial move
		docall(board, moves);
		return 1;
	      }

              unmoveChecker(board, moves, 3);
            }
          }

	  if( nMoves == 0 ) {
	    // partial move
	    docall(board, moves);
	    return 1;
	  }

          unmoveChecker(board,  moves, 2);
        }
      }

      if( nMoves == 0 ) {
	// partial move
	docall(board, moves);
	return 1;
      }
      
      unmoveChecker(board,  moves, 1);
    }
  }

  return nMoves;
}

void
MoveGenerator::playMove(EggBoard board, Emove const& move)
{
  for(int i = 1; (i < 5) && move.from[i-1]; i++) {
    int const departurePoint = move.from[i-1];
    int arrivalPoint = move.to[i-1];

    if( arrivalPoint < 0 ) {                // A blot was hit
      arrivalPoint = -arrivalPoint;
      board[1-turn][BAR-arrivalPoint]--;   // Remove the blot
      board[1-turn][BAR]++;                // and place it on the bar */
    }

    board[turn][departurePoint]--;         // Move our own checker
    board[turn][arrivalPoint]++;           // to it's landing spot.
  }
}

void
MoveGenerator::unplayMove(EggBoard board, Emove const& move)
{
  for(int i = 1; (i < 5) && move.from[i-1]; ++i) {
    int const departurePoint = move.from[i-1];
    int arrivalPoint = move.to[i-1];

    if( arrivalPoint < 0 ) {                // We hit a blot
      arrivalPoint = -arrivalPoint;
      board[1-turn][BAR-arrivalPoint]++;   // Replace the blot
      board[1-turn][BAR]--;                // remove it from the bar
    }

    board[turn][departurePoint]++;         // Replace our own checker
    board[turn][arrivalPoint]--;           // to it's original spot.  
  }
}




int
MoveGenerator::generate(Board& board, const int dice[2])
{
  EggBoard eb;
  
  eb[0][0] = eb[1][0] = 0;
  memcpy(&eb[0][1], board[0], sizeof(eb[0]) - sizeof(eb[0][0]));
  memcpy(&eb[1][1], board[1], sizeof(eb[1]) - sizeof(eb[1][0]));

  uint nMoves;
  
  if( dice[0] == dice[1] ) {
    nMoves = movesDouble(eb, dice[0]);
  } else {
    nMoves = movesNonDouble(eb, moves, dice[0], dice[1]);

    for(uint i = 0; i < nMoves; ++i) {
      Emove const& m = moves[i];
    
      playMove(eb, m);

      docall(eb, m);

      unplayMove(eb, m);
    }
  }

  return nMoves;
}


bool
MoveGenerator::play(Board&           board,
		    MoveDesc const&  move,
		    int const        side)
{
  for(uint i = 0; (i < 4) && move.desc[2*i] > 0; ++i) {
    int const departurePoint = move.desc[2*i] - 1;
    int const arrivalPoint = move.desc[2*i+1] - 1;

    if( board[side][departurePoint] == 0 ) {
      return false;
    }
    
    if( arrivalPoint >= 0 ) {
      uint& opp = board[1-side][23-arrivalPoint];
      if( opp > 1 ) {
	return false;
      }
      if( opp == 1 ) {
	opp = 0;
	board[1-side][24]++;                // and place it on the bar */
      }
    }

    board[side][departurePoint]--;         // Move our own checker
    
    if( arrivalPoint >= 0 ) {
      board[side][arrivalPoint]++;           // to it's landing spot.
    }
  }
  return true;
}
