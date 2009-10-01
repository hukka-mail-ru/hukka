// -*- C++ -*-
#if !defined( MOVES_H )
#define MOVES_H

/*
 * moves.h
 *
 * by Joseph Heled, 2000
 *
 * Eric Groleau move generator, adapted from eggrules.c. 
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

#if defined( __GNUG__ )
#pragma interface
#endif

#include "bgdefs.h"

namespace BG {

/// BackGammon move generator.
//
class MoveGenerator {
public:

  ///  Generate all moves from @arg{board} using @arg{dice}
  //
  int		generate(Board& board, const int dice[2]);

  /// Callback for each move.
  //  Called for each legal move.
  //
  //  @arg{board} is position after move, @arg{desc} is move description.
  //
  virtual void  callback(Board const& board, MoveDesc const& desc) = 0;

  /// Apply move @arg{m} to @arg{board} by @arg{side}.
  //
  //  Return false if not a legal move, true otherwise.
  //
  static bool	play(Board& board, MoveDesc const& m, int side);

private:

  // Some day, a 1000 years from now, M$ will accept the legal C++
  // static unsigned int const NB_POINTS = 26;
  
  enum { NB_POINTS = 26 };

  ///
  typedef int 	EggBoard[2][NB_POINTS];

  /// 
  struct Emove {
    /// Number of unplayed dies.
    int  unplayedDice;

    /// Inital point.
    int  from[4];

    /// Final point .
    //  If the final point is negative, this move hit 
    //  an opposing checker and sent it to the bar. 
    //
    int  to[4];
  };

  /// Convert to standard board, construct a @ref{MoveDesc} and call
  /// @ref{callback}.
  //
  void		docall(EggBoard board, Emove const& m);

  /// List of moves for non double dice.
  //
  // Simple upper limit on number of moves for a non double - at most 15
  // points contain a checker, so there are at most 15 choices for the first
  // dice, and same for the second.
  //
  Emove 	moves[15*15];


  ///
  //
  //  Try to move an active player's checker from the departure
  //  point using dice value @arg{dice}. If the move is illegal,
  //  return @samp{false} without moving the checker.
  //
  //  If the move is legal, then,
  //@itemize
  //@item  moves the checker in @arg{board}.
  //@item  records the move in @arg{move} as move number @arg{d}.
  //@item  return @samp{true}.
  //@end
  //
  //@multitable @arg
  //@item  board  @tab  current checker positions
  //@item  dice   @tab  dice value
  //@item  point  @tab  departure point
  //@item  maxPoint  @tab    highest occupied point
  //@item  move   @tab    move description
  //@item  d      @tab    current move number
  //@end
  //
  static bool	moveChecker(EggBoard  board,
			    uint      dice,
			    uint      point,
			    uint      maxPoint,
			    Emove&    move,
			    int       d);

  ///
  //  Replace a previously moved checker to it's original
  //  position using move number @arg{d} in @arg{move} description.
  //
  //  Note: checkers must be unmoved in the exact reverse order in which they
  //    were moved (@ref{moveChecker}) to keep a coherent board.
  //
  //@multitable @arg
  //@item board @tab   current checker positions
  //@item move  @tab   move description
  //@item d     @tab   current move number
  //@end
  //
  static void	unmoveChecker(EggBoard board, Emove const& move, int d);


  /// Find legal moves for a non double.
  //
  //  Find all legal moves playing dice @arg{d1}-@arg{d2} from position
  //  @arg{board}. Legal moves are written to @arg{moves}.
  //
  //@multitable @arg
  //@item board  @tab    current checker positions.
  //@item moves  @tab    array of moves
  //@item  d1
  //@itemx d2    @tab    dice value
  //@end
  //
  //  Returns number of legal moves found.
  //
  //  Note that 'board' is modified using @ref{moveChecker} and
  //  @ref{unmoveChecker} to find the valid moves. However, @arg{board}' is
  //  returned to it's original value at the end of the call, that is all
  //  checkers that are moved are subsequently unmoved.
  //
  static int    movesNonDouble(EggBoard  board,
			       Emove*    moves,
			       int       d1,
			       int       d2);

  /// Find legal moves for a double.
  //
  //  Find all legal moves playing @arg{dice}-@arg{dice} from position
  //  @arg{board}. Call @ref{callback} for each.
  //
  //  return number of legal moves found.
  //
  int		movesDouble(EggBoard board, int dice);

  ///
  //  Apply @arg{move} to @arg{board} for player. The legality of the move
  //  assumed, not verified.
  //
  static void	playMove(EggBoard board, Emove const& move);

  ///
  // restore @arg{board} to the state it was in before @arg{move} was
  //  applied.
  //
  //  Note: moves must be unplayed in the exact reverse order in which they
  // were played to keep a coherent board.
  //
  static void	unplayMove(EggBoard board, Emove const& move);

};

}

#endif
