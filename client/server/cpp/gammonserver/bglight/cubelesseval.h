// -*- C++ -*-
#if !defined( CUBELESSEVAL_H )
#define CUBELESSEVAL_H

/*
 * cubelesseval.h
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

#include "bgdefs.h"

namespace BG {

/// Evaluator for any type of backgammon position.
//
class CubelessEvaluator : public Evaluator {
public:
  CubelessEvaluator(void);

  virtual ~CubelessEvaluator();
  
  ///
  virtual void 	evaluate(Board const& b, CubelessProbs probs);

  /// Invert probabilities from X to O (or vise versa).
  //
  static void	invert(CubelessProbs probs);

  /// Return @samp{true} if game is over on @arg{board}.
  //
  static bool	gameOver(Board const& board);

protected:
  /// Position types
  //  We hold a separate evaluator for each type.
  //
  enum PositionType {
    ///  Game already finished 
    GAME_OVER = 0,
    
    ///  Both sides bearing off
    BEAROFF = 1,
    
    ///  A pure race
    RACE = 2,

    /// At least one side has less than 7 @dfn{active} checkers.
    //  @dfn{non active} - borne out or buried on the 1 to 3 points.
    //
    CRASHED,
    
    /// Any other contact
    CONTACT
  };

  ///
  // Should be

  // (HACK) (WINCE) I want
  // static uint const N_TYPES = CONTACT + 1;
  // Which is not supported on WinCE at the moment (4/Jan/2004).
  // I try to avoid ifdef's if a reasonable alternative exists.
  
  enum {
    /// Number of types. 
    N_TYPES = CONTACT + 1
  };

  /// Determine position type.
  //
  static PositionType classify(Board const& board);

  /// @ref{sanity.h,Sanity} checking of probabilities @arg{probs} generated
  /// by NN for @arg{board}.
  //
  //  Board class @arg{t} is passed as well to avoid recomputing it.
  //
  void		sanity(PositionType const  t,
		       Board const&        board,
		       CubelessProbs       probs);

  /// Call evaluator for @arg{board} of class @arg{t} and place the result
  /// in @arg{probs}.
  //
  void		evaluate(PositionType const  t,
			 Board const&        board,
			 CubelessProbs       probs);
  
  
private:
  /// Evaluator per type.
  //
  Evaluator*	perTypeEval[N_TYPES];
};

}

#endif
