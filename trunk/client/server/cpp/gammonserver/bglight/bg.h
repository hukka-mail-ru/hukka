// -*- C++ -*-
#if !defined( BG_H )
#define BG_H

/*
 * bg.h
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

#if defined( __GNUG__ )
#pragma interface
#endif


#include "systypes.h"
#include "bgdefs.h"

namespace BG {

class NoisyCubeless;

class MatchState;

/// BackGammon analyzer.
//
//  User of class need to maintain the match setup using @ref{setScore},
//  @ref{setCube} and @ref{crawford}.
//
class BackGammon {
public:
  ///
  BackGammon(void);
  
  ~BackGammon();

  /// Set match score to @arg{xAway}-@arg{oAway} away.
  //  Sets cube to 1 and crawford to false as well.
  //
  void		setScore(uint xAway, uint oAway);

  /// Set cube to @arg{cube}, with X as owner is @arg{xOwns}, O otherwise.
  //
  void		setCube(uint cube, bool xOwns);

  /// Set game as CRAWFORD .
  //
  void		crawford(bool on);

  /// Find Best move.
  //@multitable @arg
  //@item MoveDesc  @tab Best move returned.
  //@item dice      @tab Dice to play.
  //@item board     @tab The board. Side 1 is to move.
  //@item xOnPlay   @tab When true, Side 1 is X, otherwise it is O.
  //@item noise     @tab Degrade move selection by that amount. 
  //@item nPlies    @tab Serach level. Currently ignored, hardwired to 0
  //@end
  //
 
#ifdef ENABLE_BOT  
  void 		findBestMove(MoveDesc&  moveDesc,
			     const int  dice[2],
			     Board&     board,
			     bool       xOnPlay,
			     uint*      resign = 0,
			     float      noise = 0.0,
			     uint       nPlies = 0);
#endif

  bool		autoMove(Board const&   board,
			 const int      dice[2],
			 int            side,
			 MoveDesc&      desc);

  /// Should player (side 1 of board) double?
  //  When @arg{xOnPlay} is true, Side 1 is X, otherwise it is O.
  //
  bool		offerDouble(Board const& board, bool xOnPlay);
  
  /// Should player take or drop?
  //  When @arg{xOnPlay} is true, Side 1 is X, otherwise it is O.
  //
  bool		acceptDouble(Board const& board, bool xOnPlay);

  //@ Should player accept a resignation?
  //
  // Player resigning is X if @arg{xSide}, O otherwise.
  //
  // @arg{nPoints} is 1/2/3 for type of resignation.

  /// Variant 1:  player resigned after making a move
  //
  //  @arg{board} is the board after the move, from the opponent's view (i.e.
  //  resigner is side 0).
  //
  bool		resigns(Board const&  board,
			uint          nPoints,
			bool          xSide) const;

  /// Variant 2: player resigned before rolling.
  //
  //   @arg{board} is the board from the resigner point of view (i.e. resigner
  //   is side 1).
  //
  bool		resignsBeforeRoll(Board const&  board,
				  uint          nPoints,
				  bool          xSide) const;


  /// Check legality of partial move.
  //
  //@multitable @arg
  //@item board @tab The board
  //@item dice  @tab dice to play
  //@item move  @tab partial move to check
  //@item xSide @tab side to move
  //@end
  //
  // Return @code{true} if there exists a legal move who is a superset of
  // @arg{move} .
  //
  bool		partialMoveLegal(Board const&     board,
				 const int        dice[2],
				 MoveDesc const&  move,
				 uint             side);
  
  /// Check move legality.
  //@multitable @arg
  //@item board @tab The board
  //@item dice  @tab dice to play
  //@item move  @tab move to check
  //@item xSide @tab side to move
  //@end
  //
  // If move is legal, board is changed to reflect the post move position.
  // If illegal, board is untouched.
  //
  static bool		legal(Board&           board,
			      const int        dice[2],
			      MoveDesc const&  move,
			      uint             xSide);
  
  /// Game over?
  //  Return 0 for game on. Number of points won if game over (1/2/3,
  //  ignoring the cube).
  //
  int		gameOver(Board const& board);

  /// Can side @arg{xSide} double.
  //  And would he want to (i.e. cube is not dead for him).
  //
  bool		mayDouble(bool xSide) const;

  /// Current match state.
  //
  MatchState const&	state(void) const;

private:
  /// Evaluator for any backgammon position.
  //
  NoisyCubeless&	eval;

  /// Match state.
  MatchState&		match;

  /// Should player accept resignation (Helper of resign methods).
  //
  //@multitable @arg
  //@item probs   @tab position probabilities for the acceptor.
  //@item nPoints @tab type of resignation 1/2/3
  //@item xSide   @tab when true, resigner is X, otherwise O
  //@end
  //
  bool			resignOk(CubelessProbs const  probs,
				 uint                 nPoints,
				 bool                 xSide) const;
};

inline MatchState const&
BackGammon::state(void) const
{
  return match;
}

}

#endif
