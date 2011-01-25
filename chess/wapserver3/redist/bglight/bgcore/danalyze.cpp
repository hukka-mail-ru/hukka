#if defined( __GNUG__ )
#pragma implementation
#endif

/*
 * danalyze.cc
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

#include "debug.h"
#include <algorithm>
#include "danalyze.h"
#include "equities.h"

using namespace BG;
#ifdef WIN32
#include <windows.h>
#else
using std::min;
#endif


// Round 'f' to 4 significant digits
static inline float
roundFloat(float const f)
{
  float const factor = 10000.0;
  
  return long(f * factor + 0.5) / factor;
}

// Round 'f' to 4 significant digits
static inline void
roundFloatVar(float& f)
{
  f = roundFloat(f);
}

static float
estimateNR(Board const& b)
{
  uint xpip = 0, opip = 0;
  
  for(uint i = 0; i < 25; ++i) {
    if( int const c = b[0][i] ) {
      opip += (i+1) * c;
    }
    if( int const c = b[1][i] ) {
      xpip += (i+1) * c;
    }
  }

  xpip *= 2;
  opip *= 2;  
  for(uint i = 0; i < 5; ++i) {
    if( int const c = b[0][i] ) {
      opip += (5 - i) * c;
    }
    if( int const c = b[1][i] ) {
      xpip += (5 - i) * c;
    }
  }

#ifdef WIN32
  int pip = min(xpip, opip);
#else
  int pip = std::min(xpip, opip);
#endif


  return (pip / (2.0 * 8.16));
}


// Both values are empirical.

// When estimating equity for a cube > 1, this is the weight
// for the live part.
// Only for contact positions. Race has a more refined model.
// The formula is (w * live + (1-w) * dead-for-me)
//
static float const ownedWeight = 0.7;

// When estimating equity for a centered cube (i.e. 1), this is the weight
// for the live part.
// The formula is (w * live + (1-w)/2 * (dead-for-me + dead-for-opp))
//
static float const centeredWeight = 0.65;

static float
positionFdt(uint const   xAway,
	    uint const   oAway,
	    uint const   cube,
	    float const  xgr,
	    float const  ogr,
	    Board const& b)
{
  // For contact positions, use a fixed number for now.
  float l = ownedWeight;

  if( b.isRace() ) {
    Equities::Es c;
    Equities::Es d;

    Equities::get(c, &d, oAway, xAway, 2*cube, ogr, xgr);
    
    float const lp = min(c.xLow, d.xLow);
    float const gain = d.yHigh - c.yHigh;
    float const lose = c.e(lp) - d.e(lp);
    if( lose != 0.0 ) {
      float const insentive = gain/lose;
      float const magic = 0.060658192655657979;
      l = magic * estimateNR(b) * ((2 * insentive)/(insentive + 1));
    } else {
      l = 1.0;
    }
  }

  return l;
}

static float
mwcStatic(Equities::Es const&           e,
	  Equities::Es const&           ed,
	  float const                   xWins,
	  float const                   xgr,
	  float const                   ogr,
	  uint const                    cube,
	  ICC(bool const                xHoldsCube,)
	  uint const                    xAway,
	  uint const                    oAway,
	  float const                   cubeLife,
	  Board const&                  board,
	  DoubleAnalyze::Result* const  result)
{
  float e1;

  // we assume cube access
  {                                        assert( cube == 1 || xHoldsCube ); }

  float nd;

  // If outside window, code below will handle
    
  if( cube == 1 ) {
    {
      Equities::Es dead;

      dead.xLow = 0;
      dead.yLow = Equities::eWhenLose(ogr, xAway, oAway, cube);
      dead.xHigh = 1.0;
      dead.yHigh = Equities::eWhenWin(xgr, xAway, oAway, cube);
      
      float ld = 0.0;
      if( xWins >= e.xLow ) {
	Equities::Es tmp1 = dead;
	tmp1.xLow = e.xLow; tmp1.yLow = e.yLow;
      
	ld = tmp1.y(xWins);
      }

      float dl = 0.0;
      if( xWins <= e.xHigh ) {
	Equities::Es tmp1 = dead;
	tmp1.xHigh = e.xHigh; tmp1.yHigh = e.yHigh;

	dl = tmp1.y(xWins);
      }

      float ll;
      if( xWins > e.xHigh ) {
	Equities::Es tmp1 = dead;
	tmp1.xLow = e.xHigh; tmp1.yLow = e.yHigh;
	  
	ll = tmp1.y(xWins);

	dl = ll;
      } else if( xWins < e.xLow ) {
	Equities::Es tmp1 = dead;
	tmp1.xHigh = e.xLow; tmp1.yHigh = e.yLow;
	  
	ll = tmp1.y(xWins);

	ld = ll;
      } else {
	ll = e.y(xWins);
      }

      nd = (cubeLife * ll + (1-cubeLife)/2 * (dl + ld)); 
    }
  } else {
    Equities::Es tmp = e;
    tmp.xHigh = 1.0;
    tmp.yHigh = Equities::eWhenWin(xgr, xAway, oAway, cube);

    float const ndd = tmp.y(xWins);
	
    float ndl;
    if( xWins <= e.xHigh ) {
      ndl = e.y(xWins);
    } else {
      tmp.xLow = e.xHigh; tmp.yLow = e.yHigh;
	
      ndl = tmp.y(xWins);
    }
      
    float const l = positionFdt(xAway, oAway, cube, xgr, ogr, board);
      
    nd = l * ndl + (1-l) * ndd;
  }

  Equities::Es tmp = ed;
  tmp.xLow = 0; tmp.yLow = Equities::eWhenLose(ogr, xAway, oAway, 2*cube);
  float const dtd = tmp.y(xWins);
  float dtl;

  if( xWins < ed.xLow ) {
    tmp.xHigh = ed.xLow; tmp.yHigh = ed.yLow;
    dtl = tmp.y(xWins);
  } else {
    dtl = ed.y(xWins);
  }

  roundFloatVar(nd);

  float const l = positionFdt(xAway, oAway, cube, xgr, ogr, board);
    
  // if automatic redouble, full cube efficiency
  float const dbtk =
    roundFloat((xAway <= 2*cube) ? dtl : (l * dtl + (1-l) * dtd));

  float const dbdr = roundFloat(e.yHigh);

  {
    if( result ) {
      result->matchProbNoDouble = Equities::equityToProb(nd);
      result->matchProbDoubleTake = Equities::equityToProb(dbtk);
      result->matchProbDoubleDrop = Equities::equityToProb(dbdr);
    }
	
    if( dbtk < dbdr ) {
      // take
      if( nd < dbtk ) {
	// double/take
	  
	e1 = dbtk;
      } else {
	e1 = nd;
      }
    } else {
      // drop
      if( nd > dbdr ) {
	// too good
	e1 = nd;
      } else {
	e1 = dbdr;
      }
    }
  }

  return e1;
}

void
DoubleAnalyze::cubefulEquity0(Result&              result,
			      Board const&         board,
			      CubelessProbs const  p,
			      uint const           onPlayAway,
			      uint const           opAway,
			      uint const           cube,
			      bool const
#if !defined( NDEBUG )
			                           onPlayHoldsCube
#endif
  )
{
  float const xWins = p[WIN];
  float const xWinsGammon = p[WINGAMMON];
  float const oWins = 1 - xWins;
  float const oWinsGammon = p[LOSEGAMMON];

  float const oGammonRatio = (oWins > 0) ? oWinsGammon / oWins : 0.0;
  float const xGammonRatio = (xWins > 0) ? xWinsGammon / xWins : 0.0;
    
  Equities::Es e;
  Equities::Es ed;

  {                                  assert( cube == 1 || onPlayHoldsCube ); }
  
  Equities::get(e, &ed, onPlayAway, opAway, cube, xGammonRatio, oGammonRatio);

  float const cubeLife = centeredWeight;

  mwcStatic(e, ed, xWins, xGammonRatio, oGammonRatio, cube,
	    ICC(true,) onPlayAway, opAway, cubeLife, board, &result);
}

void
DoubleAnalyze::Result::setDecision(CubelessProbs const probs)
{
  actionTake = matchProbDoubleTake < matchProbDoubleDrop;
  tooGood = false;
  
  if( actionTake ) {
    if( matchProbDoubleTake >= matchProbNoDouble ) {
      actionDouble = true;
    } else {
      actionDouble = false;
    }
  } else {

    if( probs[1] == 0.0 || /* no gammons */
	matchProbDoubleDrop > matchProbNoDouble ) {
      actionDouble = true;
    } else {
      actionDouble = false;
      tooGood = true;
    }
  }
}

void
DoubleAnalyze::analyze(Result&            result,
		       Board const&       b,
		       bool const         xOnPlay,
		       MatchState const&  match)
{
  CubelessProbs probs;

  eval.evaluate(b, probs);

  uint const onPlayAway = xOnPlay ? match.xAway : match.oAway;

  // away for opponent
  uint const opAway = xOnPlay ? match.oAway : match.xAway;

  uint const cube = match.cube;

  cubefulEquity0(result, b, probs, onPlayAway, opAway, cube, true);

  result.setDecision(probs);
}


DoubleAnalyze::DoubleAnalyze(Evaluator& eval_) :
  eval(eval_)
{}
