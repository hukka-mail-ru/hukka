#if defined( __GNUG__ )
#pragma implementation
#endif

/*
 * bg.cc
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

#include <cassert>
#include <climits>
#include <cmath>
#include <string.h>
#include <algorithm>

#include "bg.h"

#include "rand.h"
#include "moves.h"
#include "cubelesseval.h"
#include "equities.h"
#include "danalyze.h"

#ifdef WIN32
#include <windows.h>
#else
using std::min;
using std::max;
#endif


namespace BG {

/// Evaluator which adds pseudo Gaussian noise to simulate weaker players. 
//
class NoisyCubeless : public CubelessEvaluator {
public:
  NoisyCubeless(void);
  
  virtual ~NoisyCubeless() {}

  /// Set noise stantard deviation.
  //
  void    setNoiseLevel(float noiseLevel);

  ///
  virtual void 	evaluate(Board const& b, CubelessProbs probs);

private:
  /// Noise stantard deviation. 
  float   	noiseLevel;
  
  /// Generate noise using Box Muller transformation.
  //
  float		box_muller(float s);

  /// Use stored value, since Box-Muller generates them in pairs.
  bool 		useSaved;

  /// 
  float 	saved;
};

NoisyCubeless::NoisyCubeless(void) :
  noiseLevel(0.0),
  useSaved(false)
{}

void
NoisyCubeless::setNoiseLevel(float const l)
{
  if( l != noiseLevel ) {
    useSaved = false;
    noiseLevel = l;
  }
}

float
NoisyCubeless::box_muller(float const s)
{
  if( useSaved )  {
    useSaved = false;
    return saved * s;
  }

  float x1, x2, w;
  do {
    x1 = 2.0 * (float(Random::get())/ULONG_MAX) - 1.0;
    x2 = 2.0 * (float(Random::get())/ULONG_MAX) - 1.0;
    w = x1 * x1 + x2 * x2;
  } while ( w >= 1.0 );

  w = sqrt( (-2.0 * log(w) ) / w );
  float const y1 = x1 * w;

  saved = x2 * w;
  useSaved = true;

  return( y1 * s );
}




void
NoisyCubeless::evaluate(Board const& b, CubelessProbs probs)
{
  PositionType const t = classify(b);
  
  CubelessEvaluator::evaluate(t, b, probs);

  if( noiseLevel > 0.0 ) {
    probs[0] += box_muller(noiseLevel);

    probs[0] = max(min(probs[0], 1.0f), 0.0f);
  }

  sanity(t, b, probs);
}

/// Finds best move for positions.
//
class PliedBestMove : private MoveGenerator {
public:
  ///
  //@multitable @arg
  //@item eval   @tab cubeless evaluator
  //@item nPlies @tab search deapth (currently ignored, hardwired to 0)
  //@item match  @tab match status
  //@item noiseLevel @tab Add a random noise from the interval
  //                      [-noiseLevel/2,+noiseLevel/2] for each move.
  //@end
  //
  PliedBestMove(CubelessEvaluator&  eval,
		uint                nPlies,
		MatchState const&   match);

  /// Find best move.
  //  For X/O (@arg{xOnPlay}) of @arg{board} using @arg{dice}.
  //
  //  Set @arg{moveBoard} and @arg{MoveDesc} to move board and description.
  //
  //  Return number of positions examined.
  //
  uint		find(Board&      board,
		     const int   dice[2],
		     bool        xOnPlay,
		     CubelessProbs p = 0);

  ///
  Board		moveBoard;
  ///
  MoveDesc	moveDesc;
private:

  ///
  CubelessEvaluator& 	eval;
  ///
  MatchState const&	match;

  float*         	sprobs;

  // currently unused
  uint const 		nPlies;

  /// Side on play
  bool		xOnPlay;

  /// Best move
  float		moveMwc;

  ///
  void  callback(Board const& board, MoveDesc const& desc);
};

PliedBestMove::PliedBestMove(CubelessEvaluator&  eval_,
			     uint                nPlies_,
			     MatchState const&   match_) :
  eval(eval_),
  match(match_),
  nPlies(nPlies_)
{
  {                                                    assert( nPlies == 0 ); }
}

uint
PliedBestMove::find(Board&        board,
		    const int     dice[2],
		    bool          xOnPlay_,
		    CubelessProbs p)
{
  moveMwc = -99;
  xOnPlay = xOnPlay_;
  moveDesc.none();
  sprobs = p;
  
  return generate(board, dice);
}


void
PliedBestMove::callback(Board const& board, MoveDesc const& desc)
{
  Board b(board, true);

  CubelessProbs probs;
  
  // !xOnPlay; for higher plies

  eval.evaluate(b, probs);
	
  eval.invert(probs);

  float const mwc = match.mwc(probs, xOnPlay);

  if( mwc >= moveMwc ) {
    moveBoard = b;

    moveDesc =  desc;
    moveMwc = mwc;

    if( sprobs ) {
      memcpy(sprobs, probs, sizeof(CubelessProbs));
    }
  }
}

BackGammon::BackGammon(void) :
  eval(*(new NoisyCubeless)),
  match(*(new MatchState))
{}

BackGammon::~BackGammon()
{
  delete &eval;
}

void
BackGammon::setScore(uint const xAway, uint const oAway)
{
  // set cube to 1, non crawford
  
  match.set(xAway, oAway, 1, false, 0);
}

void
BackGammon::setCube(uint const cube, bool const xOwns)
{
  match.set(0, 0, cube, xOwns, -1);
}

void
BackGammon::crawford(bool const on)
{
  {                           assert( match.xAway == 1 || match.oAway == 1 ); }
  match.set(0, 0, 0, false, on);
}

#ifdef ENABLE_BOT
void
BackGammon::findBestMove(MoveDesc&   moveDesc,
			 const int   dice[2],
			 Board&      board,
			 bool const  xOnPlay,
			 uint*       resign,
			 float const noise,
			 uint const  nPlies)
{
  eval.setNoiseLevel(noise);
  
  PliedBestMove m(eval, nPlies, match);

  CubelessProbs probs;
  if( resign ) {
    *resign = 0;
  }

  if( m.find(board, dice, xOnPlay, resign ? probs : 0) ) {
    board = m.moveBoard;
    moveDesc = m.moveDesc;

    if( resign ) {
      if( board.isRace() ) {
	// 2 6-6 is low enough?
	float const lowProb = 1.0 / (36.0 * 36.0);
      
	*resign = 0;

	if( probs[WIN] <= lowProb ) {
	  uint const opAway = xOnPlay ? match.oAway : match.xAway;
	  if( opAway <= match.cube ) {
	    *resign = 1;
	  } else {
    
	    if( probs[LOSEGAMMON] >= 1-lowProb ) {
	      if( probs[LOSEBACKGAMMON] == 0.0 || opAway <= 2*match.cube ) {
		*resign = 2;
	      }
	    }
	    else if( probs[LOSEGAMMON] < lowProb ) {
	      *resign = 1;
	    }
	  }
	}
      }
    }
  } else {
    // no moves
    board.swapSides();
    moveDesc.none();
  }
}
#endif

bool
BackGammon::offerDouble(Board const& board, bool const xOnPlay)
{
  if( match.cubeDead(xOnPlay) ) {
    return false;
  }
  
  {                      assert( match.cube == 1 || match.xOwns == xOnPlay ); }

  DoubleAnalyze doubleInfo(eval);
  DoubleAnalyze::Result r;
  doubleInfo.analyze(r, board, xOnPlay, match);

  return r.actionDouble;
}


bool
BackGammon::acceptDouble(Board const& board, bool const xOnPlay)
{
  DoubleAnalyze doubleInfo(eval);
  DoubleAnalyze::Result r;
  doubleInfo.analyze(r, board, !xOnPlay, match);

  return r.actionTake;
}


bool
BackGammon::resignOk(CubelessProbs const  probs,
		     uint const           nPoints,
		     bool const           xSide) const
{
  // use simplified logic. 
  //
  float const e = Equities::money(probs);

  return e <= nPoints;

  // The score match awareness code below need more work  
#if 0
  uint const xAway = match.xAway;
  uint const oAway = match.oAway;
  uint const cube = match.cube;
  
  int const rsAway = xSide ? xAway : oAway;
  int const away = xSide ? oAway : xAway;

  float const wins = probs[WIN];
  float const winsGammon = probs[WINGAMMON];
  float const rsWins = 1 - wins;
  float const rsWinsGammon = probs[LOSEGAMMON];

  float const gr = (wins > 0) ? winsGammon / wins : 0.0;
  float const rsgr = (rsWins > 0) ? rsWinsGammon / rsWins : 0.0;

  // When opp holds the cube, not correct since get it assumes X holds it
  Equities::Es now;
  Equities::get(now, 0, away, rsAway, cube, gr, rsgr, match.crawfordGame);

  if( wins > now.xHigh ) {
    return false;
  }
  
  float const e = now.e(wins);

  Equities::get(now, 0, away, rsAway, cube * nPoints, 0, 0,
		match.crawfordGame);
  float const e1 = now.e(1);


  // Since even our bearoff evaluations are based on a NN with some error,
  // allow some slack here.
  
  float const epsilon = 0.001;
  
  return (e1 >= e - epsilon);
#endif
}

bool
BackGammon::resigns(Board const&  board,
		    uint const    nPoints,
		    bool const    xSide) const
{
  CubelessProbs probs;
  eval.evaluate(board, probs);

  return resignOk(probs, nPoints, xSide);
}

bool
BackGammon::resignsBeforeRoll(Board const&  board,
			      uint const    nPoints,
			      bool const    xSide) const
{
  CubelessProbs probs;
  eval.evaluate(board, probs);

  if( probs[LOSEGAMMON] == 0.0 ) {
    return true;
  }
  
  eval.invert(probs);

  return resignOk(probs, nPoints, xSide);
}

/// Helper in making forced moves.
//
class AutoMove : public MoveGenerator {
public:
  ///
  //@multitable  @arg
  //@item board  @tab the position
  //@item dice   @tab dice to move
  //@item desc   @tab store forced move here
  //@end
  //
  AutoMove(Board const& board, const int dice[2], MoveDesc& desc);

  /// Set to true if forced move, false otherwise.
  //
  bool		ok;
private:
  MoveDesc& 	desc;
  
  ///
  void  	callback(Board const& board, MoveDesc const& desc);
};

AutoMove::AutoMove(Board const& board, const int dice[2], MoveDesc& desc_) :
  desc(desc_)
{
  desc.none();
  
  uint const nMoves = generate(const_cast<Board&>(board), dice);
  
  ok = nMoves <= 1;
}

void
AutoMove::callback(Board const&, MoveDesc const& d)
{
  desc = d;
}

bool
BackGammon::autoMove(Board const&   board,
		     const int      dice[2],
		     int const      side,
		     MoveDesc&      desc)
{
  Board b(board, side == 0);

  AutoMove a(b, dice, desc);
  
  return a.ok;
}

/// Validates a move.
//
class MoveValidator : public MoveGenerator {
public:
  ///
  //@multitable @arg
  //@item board  @tab start board
  //@item target @tab board after move
  //@item dice   @tab dice used in move
  //@end
  //
  MoveValidator(Board const& board, Board const& target, const int dice[2]);

  /// Set to true if move valid, false otherwise.
  //
  bool		ok;
private:
  //
  Board const&	target;

  ///
  void  	callback(Board const& board, MoveDesc const& desc);
};

MoveValidator::MoveValidator(Board const& board,
			     Board const& target_,
			     const int    dice[2]) :
  ok(false),
  target(target_)
{
  uint const nMoves = generate(const_cast<Board&>(board), dice);

  if( nMoves == 0 && board == target ) {
    ok = true;
  }
}

void
MoveValidator::callback(Board const& board, MoveDesc const&)
{
  if( board == target ) {
    ok = true;
  }
}

bool
BackGammon::legal(Board&           board,
		  const int        dice[2],
		  MoveDesc const&  move,
		  uint const       side)
{
  // if side 0, swap sides becaus Move Validator only moves for side 1

  Board target(board, side == 0);
  if( ! MoveGenerator::play(target, move, 1) ) {
    return false;
  }

  if( side == 0 ) {
    board.swapSides();
  }
  
  MoveValidator v(board, target, dice);

  if( v.ok ) {
    board = target;
  }

  if( side == 0 ) {
    board.swapSides();
  }

  return v.ok;
}



bool
BackGammon::partialMoveLegal(Board const&     board,
			     int const        dice[2],
			     MoveDesc const&  move,
			     uint             side)
{
  // empty always OK
  if( move.desc[0] == -1 ) {
    return true;
  }

//    uint const nUsed = (move[2] == -1) ? 1 : ((move[4] == -1) ? 2 :
//  					    ((move[6] == -1) ? 3 : 4));

//    bool const isDouble = dice[0] == dice[1];
  
  return true; // (FIXME);
  
  // if side 0, swap sides becaus Move Validator only moves for side 1

  //  Board b(board, side == 0);
  
  //    PartialMoveValidator v(b, dice, move);

  //    return v.ok;
}
  

int
BackGammon::gameOver(Board const& board)
{
  if( ! eval.gameOver(board) ) {
    return 0;
  }

  CubelessProbs probs;

  eval.evaluate(board, probs);
  
  float const e =
    (2*probs[WIN] - 1) + probs[WINGAMMON] + probs[WINBACKGAMMON] -
    probs[LOSEGAMMON] - probs[LOSEBACKGAMMON];

  return e > 0 ? int(0.5 + e) : int(-0.5 + e);
}

bool
BackGammon::mayDouble(bool const xSide) const
{
  return ! match.cubeDead(xSide);
}

}
