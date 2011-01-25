// -*- C++ -*-
#if !defined( DANALYZE_H )
#define DANALYZE_H

/*
 * danalyze.h
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
class MatchState;
}

/// Helper in backgammon cube decisions.
//
class DoubleAnalyze {
public:
  ///
  DoubleAnalyze(BG::Evaluator& eval);

  /// Result of analysis.
  //
  struct Result {
  public:
    /// Should player double?
    bool actionDouble;

    /// Should opponent take?
    bool actionTake;

    /// Player is too good?
    //  Implies !@arg{actionDouble}.
    //
    bool tooGood;

    ///@ Cubefull estimates

    /// For no double
    float matchProbNoDouble;
    
    /// For Double/Take
    float matchProbDoubleTake;
    
    /// For Double/Drop
    float matchProbDoubleDrop;

  private:
    ///
    friend class DoubleAnalyze;

    /// Set result flags (@arg{actionDouble,actionTake,tooGood} based on
    /// already computed Cubefull estimates.
    //
    void	setDecision(BG::CubelessProbs const probs);
  };

  /// Determine cube action.
  //
  //@multitable @arg
  //@item r     @tab  the result
  //@item board @tab  The position
  //@item xOnPlay @tab true when X is to move (side 1), false when side 0
  //@item match @tab  Match socre and cube.
  //@end
  //
  // Currently only 0ply is supported.
  //
  void analyze(Result&                r,
	       BG::Board const&       board,
	       bool const             xOnPlay,
	       BG::MatchState const&  match);
  
private:
  /// 
  BG::Evaluator& eval;

  ///
  void	cubefulEquity0(Result&                  result,
		       BG::Board const&         board,
		       BG::CubelessProbs const  probs,
		       uint                     onPlayAway,
		       uint                     opAway,
		       uint                     cube,
		       bool                     onPlayHoldsCube);
};


#endif
