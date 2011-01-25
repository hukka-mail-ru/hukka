#if defined( __GNUG__ )
#pragma implementation
#endif

/*
 * cubelesseval.cc
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
#include "compiler.h"
#include "cubelesseval.h"
#include "neuralnet.h"
#include "net_inputs.h"
#include "sanity.h"

using std::swap;

namespace BG {

/// Evaluator for terminal positions.
//
class GameOverEvaluator : public Evaluator {
public:
  ///
  virtual void 	evaluate(Board const& b, CubelessProbs probs);
};

void
GameOverEvaluator::evaluate(Board const& b, CubelessProbs probs)
{
  int i;

  for(i = 0; i < 5; ++i) {
    probs[i] = 0.0;
  }
  
  for(i = 0; i < 25 && !b[0][i]; ++i) {}

  if( i == 25 ) {
    // opponent has no pieces on board; player has lost

    int c = 0;
    for(i = 0; i < 25; ++i) {
      c += b[1][i];
    }

    if( c > 14 ) {
      // player still has all pieces on board; loses gammon
      probs[LOSEGAMMON] = 1.0;

      for(i = 18; i < 25; ++i) {
	if( b[1][i] ) {
	  // player still has pieces in opponent's home board;
	  // loses backgammon 
	  probs[LOSEBACKGAMMON] = 1.0;
	  break;
	}
      }
    }
    
  } else {
    
    // player has no pieces on board; wins
    
    probs[WIN] = 1.0;
    
    int c = 0;
    for(i = 0; i < 25; ++i) {
      c += b[0][i];
    }

    if( c > 14 ) {
      // opponent still has all pieces on board; win gammon
      probs[WINGAMMON] = 1.0;

      for( i = 18; i < 25; i++ ) {
	if( b[0][i] ) {
	  // opponent still has pieces in player's home board
	  // win backgammon 
	  probs[WINBACKGAMMON] = 1.0;
	  break;
	}
      }
    }
  }
}


/// Evaluator based on a Neural Net.
//
class NeuralNetEvaluator : public Evaluator {
private:
  /// Function computing Neural Net @arg{inputs} for @arg{b}.
  //
  typedef void (*GetInputs)(Board const& b, float* inputs);
public:

  ///
  //  Evaluator 
  NeuralNetEvaluator(GetInputs i, const NeuralNet* n);

  ///
  virtual void 	evaluate(Board const& b, CubelessProbs probs);
private:

  /// Computes Neural Net inputs for position.
  GetInputs		getInputs;

  /// Neural Net evaluator.
  const NeuralNet* 	net;
};

NeuralNetEvaluator::NeuralNetEvaluator(GetInputs i, const NeuralNet* n) :
  getInputs(i),
  net(n)
{}

void
NeuralNetEvaluator::evaluate(Board const& b, CubelessProbs probs)
{
  LOCAL_ARRAY(float, inputs, net->numInputs());
  
  getInputs(b, inputs);
  net->evaluate(inputs, probs);
}


CubelessEvaluator::CubelessEvaluator(void)
{
  perTypeEval[GAME_OVER] = new GameOverEvaluator;
  
  perTypeEval[BEAROFF] =
    new NeuralNetEvaluator( bearoffGetInputs, bearoffGetNet() );
  
  perTypeEval[RACE] =
    new NeuralNetEvaluator( raceGetInputs, raceGetNet() );
  
  perTypeEval[CRASHED] =
    new NeuralNetEvaluator( crashedGetInputs, crashedGetNet() );
  
  perTypeEval[CONTACT] =
    new NeuralNetEvaluator( contactGetInputs, contactGetNet() );
}

CubelessEvaluator::~CubelessEvaluator(void)
{
  for(uint k = 0; k < N_TYPES; ++k) {
    delete perTypeEval[k];
  }
}


CubelessEvaluator::PositionType
CubelessEvaluator::classify(Board const& board)
{
  int nOppBack;

  for(nOppBack = 24; nOppBack >= 0; --nOppBack) {
    if( board[0][nOppBack] ) {
      break;
    }
  }

  int nBack;
  for(nBack = 24; nBack >= 0; --nBack) {
    if( board[1][nBack] ) {
      break;
    }
  }

  if( nBack < 0 || nOppBack < 0 ) {
    return GAME_OVER;
  }
  
  if( nBack + nOppBack > 22 ) {
    uint const N = 6;
    
    for(uint side = 0; side < 2; ++side) {
      const uint* const b = board[side];
      
      uint tot = 0;
      for(uint i = 0;  i < 25; ++i) {
	tot += b[i];
      }

      if( tot <= N ) {
	return CRASHED;
      } else {
	if( b[0] > 1 ) {
	  if( (tot - b[0]) <= N ) {
	    return CRASHED;
	  } else {
	    if( b[1] > 1 && (1 + tot - (b[0] + b[1])) <= N ) {
	      return CRASHED;
	    }
	  }
	} else {
	  if( (int(tot) - (b[1] - 1)) <= int(N) ) {
	    return CRASHED;
	  }
	}
      }
    }
    
    return CONTACT;
  }
  
  if( nBack > 5 || nOppBack > 5 ) {
    return RACE;
  }

  return BEAROFF;
}

void
CubelessEvaluator::sanity(PositionType const  t,
			  Board const&        b,
			  CubelessProbs       probs)
{
  switch( t ) {
    case RACE:
    {
      raceSanity(b, probs, *perTypeEval[BEAROFF]);
      break;
    }
    default:
    {
      genericSanity(b, probs);
      break;
    }
  }
}

void
CubelessEvaluator::evaluate(PositionType const  t,
			    Board const&        b,
			    CubelessProbs       probs)
{
  perTypeEval[t]->evaluate(b, probs);
}

void
CubelessEvaluator::evaluate(Board const& b, CubelessProbs probs)
{
  PositionType const t = classify(b);
  
  evaluate(t, b, probs);
  
  sanity(t, b, probs);
}


void
CubelessEvaluator::invert(CubelessProbs probs)
{
  probs[WIN] = 1.0 - probs[WIN];

  swap(probs[WINGAMMON], probs[LOSEGAMMON]);
  swap(probs[WINBACKGAMMON], probs[LOSEBACKGAMMON]);
}

bool
CubelessEvaluator::gameOver(Board const& board)
{
  return classify(board) == GAME_OVER;
}

}
