// -*- C++ -*-
#if !defined( EQUITIES_H )
#define EQUITIES_H

/*
 * equities.h
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

#include <cassert>

namespace BG {
/// Keeps match score and related information.
//
class MatchState {
public:
  MatchState(void);

  /// Set match score.
  //
  bool	set(uint  xAway,
	    uint  oAway,
	    uint  cube,
	    bool  xOwns,
	    int   crawford);

  /// Cube dead for this setup?
  //
  inline bool cubeDead(void) const;

  /// Cube dead for player?
  //  Either player has no cube access, or it is dead for him.
  //
  inline bool cubeDead(bool xPlayer) const;
  
  /// X away in match.
  //  Number of points X needs to win the match.
  //
  uint	xAway;
  
  /// O away in match.
  //  Number of points O needs to win the match.
  uint  oAway;

  /// Current cube.
  uint	cube;

  /// True when X owns cube.
  //  Applicable only when cube > 1.
  //
  bool  xOwns;

  /// True when CRAWFORD game.
  bool	crawfordGame;

  /// Turn cubeless probabilities to match equity using market window.
  //  If money game, use gammon weights. If match, use @ref{mwcMatch}.
  //
  float	mwc(CubelessProbs const p, bool xOnPlay) const;
  
private:

  /// Turn cubeless probabilities to match equity using gammon weights.
  //  Fast but not score sensitive.
  //
  float	matchEquity(CubelessProbs const p, bool xOnPlay) const;

  /// Turn cubeless probabilities to match equity using market window.
  //  Slower but score sensitive.
  //
  float	mwcMatch(CubelessProbs const ar, bool xOnPlay) const;
  
  /// Compute Gammon/Backgammon weights for currect score and cube.
  //
  void	recompute(void);

  //@ Gammon and Backgammon cube weights.
  //
  // Let @eqn{ p_win,p_gwin,p_bgwin,p_lose,p_glose,p_bglose} be the Cubeless
  // probablities. Remembers that by convension @eqn{p_win} is inclusive of
  // @eqn{p_gwin and p_bgwin}, and @eqn{p_gwin} is inclusive of @eqn{p_bgwin}.
  //
  // For money games we have
  // @eqn{Equity = p_win + p_gwin + p_bgwin - (p_lose + p_glose + p_bglose)}
  //
  // This generalizes to match play by introducing the
  // @eqn{x_2,x_3,o_2,o_3} coefficients,
  //
  // @eqn{Match Equity = p_win + p_gwin * w_2 + p_bgwin * w_3 -
  // (p_lose + p_glose * l_2 + p_bglose * l_3)}
  //
  // (So obviously money is a special case with
  // @eqn{w_2 = w_3 = 1, l_2 = l_3 = -1}. 
  //

  ///
  float xCube2;
  ///
  float xCube3;
  ///
  float oCube2;
  ///
  float oCube3;
};

inline bool
MatchState::cubeDead(void) const
{
  return ( cube != 1 && cube >= (xOwns ? xAway : oAway) );
}

inline bool
MatchState::cubeDead(bool const xPlayer) const
{
  if( crawfordGame ) {
    return true;
  }

  // If cube centered, dead if player is 1 away.
  // If not, dead if !xPlayer owns it, or cube is high enough to win the match
  
  return ( (cube == 1 && (xPlayer ? xAway : oAway) <= 1) ||
	   (cube > 1 && (xPlayer != xOwns ||
			 cube >= (xOwns ? xAway : oAway))) );
}

}

namespace Equities {
/// Equities graph.
//  x axis is probability (x @in [01]), y is equity (y @in [-1 +1]).
//
//  Valid between @arg{xLow} and @arg{xHigh}.
//
//  Given as two endpoints of a stright line.
//  The low point is X drop point, the high is X cash-out point.
//  
struct Es {
  //@ low point

  ///
  float xLow;
  ///
  float yLow;

  //@ High point.

  ///
  float xHigh;
  ///
  float yHigh;

  /// Equity at @arg{x}.
  //  @arg{x} must be inside market window.
  //
  float y(float x) const;

  /// Probability at which equity is @arg{y}.
  //
  float x(float y) const;

  /// Equity at @arg{x}, for any @arg{x}.
  //  Return value of cash/drop-point for @arg{x} outside of market window.
  //
  float e(float x) const;

  /// @arg{x} as percent in window.
  //
  float r(float x) const;
    
  /// Reverse graph to O point of view.
  //
  void	reverse(void);
};

  
/// Get equity at current and doubled cube for non crawford games.
//
//@multitable @arg
//@item now       @tab  current equity
//@item doubled   @tab  doubled equity (when not null)
//@item  xAway
//@itemx oAway    @tab  Match score
//@item  cube     @tab  Cube value. When cube > 1, X holds it.
//@item  xGammonRatio
//@itemx oGammonRatio @tab Gammon rate as ratio of total wins (r @in [01])
//@end
//
// If any away is 1, it is assumed to be a post crawford score.
//
void	get(Es&    now,
	    Es*    doubled,
	    uint   xAway,
	    uint   oAway,
	    uint   cube,
	    float  xGammonRatio,
	    float  oGammonRatio
#if defined( DEBUG )
	    ,int   debug
#endif
  );

/// Get equity at current and doubled cube for any score.
//  Same as above, with @arg{crawfordGame} to indicate a crawford game.
//
void	get(Es&    now,
	    Es*    doubled,
	    uint   xAway,
	    uint   oAway,
	    uint   cube,
	    float  xGammonRatio,
	    float  oGammonRatio,
	    bool   crawfordGame
#if defined( DEBUG )
	    ,int   debug
#endif
  );
  
void
getCrawfordEq(Es& now, uint away, float gammonRatio);
  
void
getCrawfordEq(Es&          e,
	      uint const   xAway,
	      uint const   oAway,
	      float const  xgr,
	      float const  ogr);

  
/// Match equity at @arg{xAway},@arg{oAway}.
//  A 0 (or negative) away value is same as @dfn{win}.
//
inline float value(int xAway, int oAway);
  
/// Match winning probablity at @arg{xAway},@arg{oAway}.
//
inline float prob(int xAway, int oAway);

/// Convert probablity to equity.
//
inline float probToEquity(float p);

/// Convert equity to probablity.
//
inline float equityToProb(float e);

/// Match winning probablity at @arg{xAway},@arg{oAway}.
//  If !@arg{crawford} and any away is 1, return probablity for a post
//  crawford score.
//
float prob(int xAway, int oAway, bool crawford);

/// Convert cubeless probablities to match wins for current score.
//  Use a simple model. Compute market window for match score using gammon
//  rates from evaluation, then get value from window.
//
//  If outside of window, assume play without turning the cube, when
//  cashing out at the appropriate point.
//
float 	mwc(const float* p, bool xOnPlay);

  
/// Probability of winning (xAway,1-away), post crawford.
//
float		probPost(int xAway);
  
/// Equity of winning (@arg{away},1-away), post crawford.
//
inline float		ePost(int away);

//@ Next four (eqWhen*) assume all away's are > 1

///  X Equity, assuming he wins, at @arg{xAway},@arg{oAway}, with given
///  cube and gammon ratio.
//
inline
float		eqWhenWin(float  xGammonRatio,
			  uint   xAway,
			  uint   oAway,
			  uint   cube);

/// X Equity, assuming he wins, at @arg{xAway},@arg{oAway}, with given cube
/// and Gammon/Backgammon ratios.
//
inline
float		eqWhenWin(float  xGammonRatio,
			  float  xBGammonRatio,
			  uint   xAway,
			  uint   oAway,
			  uint   cube);

/// X Equity, assuming he loses, at @arg{xAway},@arg{oAway}, with given cube
/// and opponent gammon ratio.
//
inline
float		eqWhenLose(float  oGammonRatio,
			   uint   xAway,
			   uint   oAway,
			   uint   cube);
  
/// X Equity, assuming he loses, at @arg{xAway},@arg{oAway}, with given cube
/// and Gammon/Backgammon ratios.
//
inline
float		eqWhenLose(float  oGammonRatio,
			   float  oBGammonRatio,
			   uint   xAway,
			   uint   oAway,
			   uint   cube);
  
/// X Equity, assuming he wins, at @arg{xAway},1 away post crawford,
///  with given cube and gammon ratio.
//
inline
float		eWhenWinPost(float  xGammonRatio,
			     uint   xAway,
			     uint   cube);

/// X Equity, assuming he wins, at @arg{xAway},1 away post crawford,
///  with given cube and Gammon/Backgammon ratios.
//
inline
float		eWhenWinPost(float  xGammonRatio,
			     float  xBGammonRatio,
			     uint   xAway,
			     uint   cube);

// No eWhenLosePost, since he simply loses the match

//@ Next four (eWhen*) assume that if any away's is 1, it is post crawford.

///  X Equity, assuming he wins, at @arg{xAway},@arg{oAway}, with given
///  cube and gammon ratio.
//
inline
float		eWhenWin(float  xGammonRatio,
			 uint   xAway,
			 uint   oAway,
			 uint   cube);

/// X Equity, assuming he wins, at @arg{xAway},@arg{oAway}, with given cube
/// and Gammon/Backgammon ratios.
//
inline
float		eWhenWin(float  xGammonRatio,
			 float  xBGammonRatio,
			 uint   xAway,
			 uint   oAway,
			 uint   cube);
  
/// X Equity, assuming he loses, at @arg{xAway},@arg{oAway}, with given cube
/// and Gammon/Backgammon ratios.
//
inline
float		eWhenLose(float  oGammonRatio,
			  uint   xAway,
			  uint   oAway,
			  uint   cube);
  
/// X Equity, assuming he loses, at @arg{xAway},@arg{oAway}, with given cube
/// and Gammon/Backgammon ratios.
//
inline
float		eWhenLose(float  oGammonRatio,
			  float  oBGammonRatio,
			  uint   xAway,
			  uint   oAway,
			  uint   cube);

// Equity table used.
extern float const equitiesTable[25][25];


/// Money match equity from cubeless @arg{p}.
inline float
money(BG::CubelessProbs const probs)
{
  return (2*probs[BG::WIN] - 1) +
    probs[BG::WINGAMMON] + probs[BG::WINBACKGAMMON] -
    (probs[BG::LOSEGAMMON] + probs[BG::LOSEBACKGAMMON]);
}

}

inline float
Equities::Es::y(float const x) const
{
  {                                        assert( xLow <= x && x <= xHigh ); }

  return yLow + ((yHigh - yLow)/(xHigh - xLow)) * (x - xLow);
}



inline float
Equities::Es::x(float const y) const
{
  {                                        assert( yLow <= y && y <= yHigh ); }
  
  return xLow + (y - yLow) * ((xHigh - xLow) / (yHigh - yLow));
}

inline float
Equities::Es::e(float const x) const
{
  if( x <= xLow ) {
    return yLow;
  }

  if( x >= xHigh ) {
    return yHigh;
  }

  return y(x);
}

inline float
Equities::Es::r(float const x) const
{
  return (x - xLow) / (xHigh - xLow);
}

inline void
Equities::Es::reverse(void)
{
  float const xl = xLow;
  xLow = 1.0 - xHigh;
  xHigh = 1.0 - xl;

  float const yl = yLow;

  yLow = -yHigh;
  yHigh = -yl;
}


inline float
Equities::probToEquity(float const p)
{
  return 2.0 * p - 1.0;
}

inline float
Equities::equityToProb(float const e)
{
  return (1 + e) / 2.0;
}


inline float
Equities::prob(int const a1, int const a2)
{
  if( a1 <= 0 ) {
    return 1.0;
  }

  if( a2 <= 0 ) {
    return 0.0;
  }
  {                                           assert( a1 <= 25 && a2 <= 25 ); }

  return equitiesTable[a1-1][a2-1];
}

inline float
Equities::value(int const a1, int const a2)
{
  return probToEquity( prob(a1, a2) );
}  

inline float
Equities::eqWhenWin(float const  xGammonRatio,
		    uint const   xAway,
		    uint const   oAway,
		    uint const   cube)
{
  {                                       assert( xAway != 1 && oAway != 1 ); }
  
  return ((1 - xGammonRatio) * value(xAway - cube, oAway) +
	  xGammonRatio * value(xAway - 2*cube, oAway));
}

inline float
Equities::eqWhenWin(float const  xGammonRatio,
		    float const  xBGammonRatio,
		    uint const   xAway,
		    uint const   oAway,
		    uint const   cube)
{
  {                                       assert( xAway != 1 && oAway != 1 ); }
  
  return ((1 - xGammonRatio) * value(xAway - cube, oAway) +
	  xGammonRatio *
	  ( (1-xBGammonRatio) * value(xAway - 2*cube, oAway) +
	    xBGammonRatio * value(xAway - 3*cube, oAway)));
}

inline float
Equities::eqWhenLose(float const  oGammonRatio,
		     uint const   xAway,
		     uint const   oAway,
		     uint const   cube)
{
  {                                       assert( xAway != 1 && oAway != 1 ); }
  
  return ((1 - oGammonRatio) * value(xAway, oAway - cube) +
	  oGammonRatio * value(xAway, oAway - 2*cube));
}

inline float
Equities::eqWhenLose(float const  oGammonRatio,
		     float const  oBGammonRatio,
		     uint const   xAway,
		     uint const   oAway,
		     uint const   cube)
{
  {                                       assert( xAway != 1 && oAway != 1 ); }
  
  return ((1 - oGammonRatio) * value(xAway, oAway - cube) +
	  oGammonRatio *
	  ( (1-oBGammonRatio) * value(xAway, oAway - 2*cube) +
	    oBGammonRatio * value(xAway, oAway - 3*cube)));
}

inline float
Equities::ePost(int const away)
{
  return 2.0 * probPost(away) - 1.0;
}

inline float
Equities::eWhenWinPost(float const  xGammonRatio,
		       uint const   xAway,
		       uint const   cube)
{
  float const e1 = ePost(xAway - cube);
  float const e2 = ePost(xAway - 2*cube);
  
  return (1 - xGammonRatio) * e1 + xGammonRatio * e2;
}

inline float
Equities::eWhenWinPost(float const  xGammonRatio,
		       float const  xBGammonRatio,
		       uint const   xAway,
		       uint const   cube)
{
  float const e1 = ePost(xAway - cube);
  float const e2 = ePost(xAway - 2*cube);
  float const e3 = ePost(xAway - 3*cube);

  return ((1 - xGammonRatio) * e1 +
	  xGammonRatio * ((1-xBGammonRatio) * e2 + xBGammonRatio * e3) );
}

inline float
Equities::eWhenWin(float const  xGammonRatio,
		   uint const   xAway,
		   uint const   oAway,
		   uint const   cube)
{
  if( xAway == 1 ) {
    return 1;
  }
  
  if( oAway == 1 ) {
    return eWhenWinPost(xGammonRatio, xAway, cube);
  }
  return eqWhenWin(xGammonRatio, xAway, oAway, cube);
}

inline float
Equities::eWhenWin(float const  xGammonRatio,
		   float const  xBGammonRatio,
		   uint const   xAway,
		   uint const   oAway,
		   uint const   cube)
{
  if( xAway == 1 ) {
    return 1;
  }
  
  if( oAway == 1 ) {
    return eWhenWinPost(xGammonRatio, xBGammonRatio, xAway, cube);
  }
  return eqWhenWin(xGammonRatio, xBGammonRatio, xAway, oAway, cube);
}

inline float
Equities::eWhenLose(float const  oGammonRatio,
		    uint const   xAway,
		    uint const   oAway,
		    uint const   cube)
{
  if( oAway == 1 ) {
    return -1;
  }
  
  if( xAway == 1 ) {
    // assumes symetry
    return -eWhenWinPost(oGammonRatio, oAway, cube);
  }
  return eqWhenLose(oGammonRatio, xAway, oAway, cube);
}


inline float
Equities::eWhenLose(float const  oGammonRatio,
		    float const  oBGammonRatio,
		    uint const   xAway,
		    uint const   oAway,
		    uint const   cube)
{
  if( oAway == 1 ) {
    return -1;
  }
  
  if( xAway == 1 ) {
    // assumes symetry
    return -eWhenWinPost(oGammonRatio, oBGammonRatio, oAway, cube);
  }
  return eqWhenLose(oGammonRatio, oBGammonRatio, xAway, oAway, cube);
}

#if defined( DEBUG )
class ostream;

ostream&
operator <<(ostream& s, Equities::Es const& e);
#endif

#endif
