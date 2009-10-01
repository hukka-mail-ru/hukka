#include "bgdefs.h"

#include "sanity.h"

enum {
  G_POSSIBLE = 0x1,
  BG_POSSIBLE = 0x2,
  OG_POSSIBLE = 0x4,
  OBG_POSSIBLE = 0x8
};

namespace BG {

void
genericSanity(Board const& board, CubelessProbs probs)
{
  int totMen[2] = {0,0};
  int back[2];
    
  for(uint side = 0; side < 2; ++side) {
    const uint* const b = board[side];

    for(uint i = 0; i < 25; ++i) {
      if( b[i] ) {
	back[side] = i;
	totMen[side] += b[i];
      }
    }
  }

  bool const contact =  back[0] + back[1] >= 24;

  if( totMen[0] < 15 ) {
    // Opponent has borne off; no gammons or backgammons possible
    probs[WINGAMMON] = probs[WINBACKGAMMON ] = 0.0;
  }
  else if( !contact && back[0] < 18 ) {
    // Opponent is out of home board; no backgammons possible
    probs[WINBACKGAMMON] = 0.0;
  }

  if( totMen[1] < 15 ) {
    // Player has borne off; no gammons or backgammons possible 
    probs[LOSEGAMMON] = probs[LOSEBACKGAMMON] = 0.0;
  } else if( !contact && back[1] < 18 ) {
    // Player is out of home board; no backgammons possible
    probs[ LOSEBACKGAMMON ] = 0.0;
  }

  if( probs[WINGAMMON] > probs[WIN] ) {
    probs[WINGAMMON] = probs[WIN];
  }

  {
    float lose = 1.0 - probs[WIN];
    if( probs[LOSEGAMMON] > lose ) {
      probs[LOSEGAMMON] = lose;
    }
  }
  
  // Backgammons cannot exceed gammons 
  if( probs[ WINBACKGAMMON ] > probs[ WINGAMMON ] )
    probs[ WINBACKGAMMON ] = probs[ WINGAMMON ];
    
  if( probs[ LOSEBACKGAMMON ] > probs[ LOSEGAMMON ] )
    probs[ LOSEBACKGAMMON ] = probs[ LOSEGAMMON ];

  {
    float noiseLevel = 1/10000.0;

    for(uint i = WINGAMMON; i < 5; ++i) {
      if( probs[i] < noiseLevel ) {
	probs[i] = 0.0;
      }
    }
  }
}

void
raceSanity(Board const& board, CubelessProbs probs, Evaluator& bearoff)
{
  int totMen0 = 0, totMen1 = 0;
  int any = 0;
    
  const uint* const b0 = board[0];
  const uint* const b1 = board[1];
  
  for(int i = 23; i >= 0; --i) {
    totMen0 += b0[i];
    totMen1 += b1[i];
  }

  if( totMen1 == 15 ) {
    any |= OG_POSSIBLE;
  }
  if( totMen0 == 15 ) {
    any |= G_POSSIBLE;
  }

  if( any ) {
    if( any & OG_POSSIBLE ) {
      for(uint i = 23; i >= 18; --i) {
	if( b1[i] > 0 ) {
	  any |= OBG_POSSIBLE;
	  break;
	}
      }
    }

    if( any & G_POSSIBLE ) {
      for(uint i = 23; i >= 18; --i) {
	if( b0[i] > 0 ) {
	  any |= BG_POSSIBLE;
	  break;
	}
      }
    }
  }
    
  if( any & (BG_POSSIBLE | OBG_POSSIBLE) ) {
    int side = (any & BG_POSSIBLE) ? 1 : 0;

    int totMenHome = 0;
    int totPipsOp = 0;

    for(uint i = 0; i < 6; ++i) {
      totMenHome += board[side][i];
    }
      
    for(uint i = 23; i >= 18; --i) {
      totPipsOp += board[1-side][i] * (i-17);
    }

    if( (totMenHome + 3) / 4 - (side == 1 ? 1 : 0) <= (totPipsOp + 2) / 3 ) {
      Board dummy;
      CubelessProbs p;
	
      for(uint i = 0; i < 25; ++i) {
	dummy[side][i] = board[side][i];
      }

      for(uint i = 0; i < 6; ++i) {
	dummy[1-side][i] = board[1-side][18+i];
      }
      for(uint i = 6; i < 25; ++i) {
	dummy[1-side][i] = 0;
      }

      bearoff.evaluate(dummy, p);

      if( side == 1 ) {
	probs[WINBACKGAMMON] = p[0];
      } else {
	probs[LOSEBACKGAMMON] = 1 - p[0];
      }
    } else {
      if( side == 1 ) {
	probs[WINBACKGAMMON] = 0.0;
      } else {
	probs[LOSEBACKGAMMON] = 0.0;
      }
    }  
  }

  genericSanity(board, probs);
}

}
