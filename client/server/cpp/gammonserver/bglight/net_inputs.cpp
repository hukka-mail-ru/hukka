#if defined( __GNUG__ )
#pragma implementation
#endif

/*
 * net_inputs.cc
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
#include "net_inputs.h"

using namespace BG;

typedef uint HalfBoard[25];


// bearoff inputs
enum {
  BI_OFF = 6*6,

  BI_PIP = BI_OFF + 14,
  
  HALF_BEAROFF_INPUTS
};

void
bearoffGetInputs(Board const& b, float* const inputs)
{
  for(unsigned int side = 0; side < 2; ++side) {
    const uint* const board = b[side];
    
    float* const afInput = inputs + side * HALF_BEAROFF_INPUTS;

    // Points 
    uint k = 0;
      
    for(int i = 0; i < 6; ++i) {
      unsigned int const nc = board[i];

      afInput[ k++ ] = nc == 1;
      afInput[ k++ ] = nc == 2;
      afInput[ k++ ] = nc == 3;
      afInput[ k++ ] = nc == 4;
      afInput[ k++ ] = nc == 5;
      afInput[ k++ ] = nc > 5 ? ( nc - 5 ) / 2.0 : 0.0;
    }

    // Men off
    {
      uint menOff = 15;
      
      for(uint i = 0; i < 6; ++i) {
	menOff -= board[i];
      }

      for(uint k = 0; k < 14; ++k) {
	afInput[ BI_OFF + k ] = menOff > k ? 1.0 : 0.0;
      }
    }
    
    {
      uint np = 0;
      
      for(int i = 0; i < 6; ++i) {
	if( uint const nc = board[i] ) {
	  np += nc * (i+1);
	}
      }
      
      afInput[BI_PIP] = np / (15.0 * 6);
    }
  }
}



// Race inputs
enum {
  RI_OFF = 92,

  RI_NCROSS = 92 + 14,
  
  HALF_RACE_INPUTS
};

void
raceGetInputs(Board const& b, float* const inputs)
{
  for(unsigned int side = 0; side < 2; ++side) {
    const uint* const board = b[side];
    
    float* const afInput = inputs + side * HALF_RACE_INPUTS;

    {                             assert( board[24] == 0 && board[23] == 0 ); }

    uint menOff = 15;
    
    // Points 
    for(uint i = 0; i < 23; ++i) {
      uint const nc = board[i];

      uint k = i * 4;
      
      afInput[ k++ ] = nc == 1;
      afInput[ k++ ] = nc == 2;
      afInput[ k++ ] = nc >= 3;
      afInput[ k ] = nc > 3 ? ( nc - 3 ) / 2.0 : 0.0;
      
      menOff -= nc;
    }

    // Men off
    for(uint k = 0; k < 14; ++k) {
      afInput[ RI_OFF + k ] = (menOff == (k+1)) ? 1.0 : 0.0;
    }
    
    uint nCross = 0;
      
    for(uint k = 1; k < 4; ++k) {
      for(uint i = 6*k; i < 6*k + 6; ++i) {
	if( uint const nc = board[i] ) {
	  nCross += nc * k;
	}
      }
    }
      
    afInput[RI_NCROSS] = nCross / 10.0;
  }
}


static void
basicContactInputs(Board const& b, float* const inputs)
{
    
  for(uint j = 0; j < 2; ++j ) {
    float* const afInput = inputs + j * 25*4;
    const uint* const board = b[j];
    
    // Points
    for(uint i = 0; i < 24; ++i) {
      int const nc = board[i];

      uint k = i * 4;
      afInput[ k++ ] = nc == 1;
      afInput[ k++ ] = nc == 2;
      afInput[ k++ ] = nc >= 3;
      afInput[ k   ] = nc > 3 ? ( nc - 3 ) / 2.0 : 0.0;
    }

    // Bar 
    {
      int const nc = board[24];
      
      afInput[ 24 * 4 + 0 ] = nc >= 1;
      afInput[ 24 * 4 + 1 ] = nc >= 2; /**/
      afInput[ 24 * 4 + 2 ] = nc >= 3;
      afInput[ 24 * 4 + 3 ] = nc > 3 ? ( nc - 3 ) / 2.0 : 0.0;
    }
  }
}



// Contact inputs -- see Berliner for some of these
enum {
  I_OFF1 = 0, I_OFF2, I_OFF3,
  
  //   Minimum number of pips required to break contact.
  //
  //       For each checker x, N(x) is checker location,
  //       C(x) is max({forall o : N(x) - N(o)}, 0)
  //
  //       Break Contact : (sum over x of C(x)) / 152
  //
  //       152 is dgree of contact of start position.
  
  I_BREAK_CONTACT,

  // Location of back checker (Normalized to [01])
  I_BACK_CHEQUER,

  // Location of most backward anchor.  (Normalized to [01])
  I_BACK_ANCHOR,

  //  Forward anchor in opponents home.
  //  Normalized in the following way:  If there is an anchor in opponents
  //  home at point k (1 <= k <= 6), value is k/6. Otherwise, if there is an
  //  anchor in points (7 <= k <= 12), take k/6 as well. Otherwise set to 2.
  //
  //  This is an attempt for some continuity, since a 0 would be the "same" as
  //  a forward anchor at the bar.
   
  I_FORWARD_ANCHOR,

  //   Average number of pips opponent loses from hits.
  //     
  //   Some heuristics are required to estimate it, since we have no idea what
  //   the best move actually is.
  //
  //   1. If board is weak (less than 3 anchors), don't consider hitting on
  //      points 22 and 23.
  //   2. Dont break anchors inside home to hit.
   
  I_PIPLOSS, 

  // Number of rolls that hit at least one checker.
  I_P1,

  // Number of rolls that hit at least two checkers.
  I_P2,

  // How many rolls permit the back checker to escape (Normalized to [01])
  I_BACKESCAPES,

  // Maximum containment of opponent checkers, from our points 9 to op back 
  // checker.
  //
  // Value is (1 - n/36), where n is number of rolls to escape.
  //
  I_ACONTAIN,
  
  // Above squared
  I_ACONTAIN2,

  // Maximum containment, from our point 9 to home.
  // Value is (1 - n/36), where n is number of rolls to escape.
  //
  I_CONTAIN,

  // Above squared
  I_CONTAIN2,


  // For all checkers out of home, 
  //   sum (Number of rolls that let x escape * distance from home)
  //
  // Normalized by dividing by 3600.
  I_MOBILITY,

  // One sided moment.
  //   Let A be the point of weighted average: 
  //   A = sum of N(x) for all x) / nCheckers.
  //
  //   Then for all x : A < N(x), M = (average (N(X) - A)^2)
  //
  //   Diveded by 400 to normalize. 

  I_MOMENT2,

  // Average number of pips lost when on the bar. Normalized to [01]
  
  I_ENTER,

  // Probablity of one checker not entering from bar.
  // 1 - (1 - n/6)^2, where n is number of closed points in op home.

  I_ENTER2,

  I_TIMING,
  
  I_BACKBONE,

  I_BACKG,
  
  I_BACKG1,

  I_FREEPIP,

  I_BACKRESCAPES,

  HALF_INPUTS
};


static int anEscapes[ 0x1000 ];

void ComputeTable0(void)
{
  for(uint i = 0; i < 0x1000; ++i) {
    uint c = 0;
	
    for(uint n0 = 0; n0 <= 5; n0++ )
      for(uint n1 = 0; n1 <= n0; n1++ )
	if( !( i & ( 1 << ( n0 + n1 + 1 ) ) ) &&
	    !( ( i & ( 1 << n0 ) ) && ( i & ( 1 << n1 ) ) ) )
	  c += ( n0 == n1 ) ? 1 : 2;
	
    anEscapes[ i ] = c;
  }
}

static int
Escapes(HalfBoard const board, uint const n)
{
  int af = 0;
    
  for(uint i = 0; i < 12 && i < n; i++){
    if( board[ 24 - n + i ] > 1 ) {
      af |= ( 1 << i );
    }
  }
    
  return anEscapes[ af ];
}


static int anEscapes1[ 0x1000 ];

static void
ComputeTable1(void)
{
  anEscapes1[ 0 ] = 0;
  
  for(uint i = 1; i < 0x1000; i++ ) {
    uint c = 0;

    uint low = 0;
    
    while( ! (i & (1 << low)) ) {
      ++low;
    }
    
    for(uint n0 = 0; n0 <= 5; n0++ )
      for(uint n1 = 0; n1 <= n0; n1++ ) {
	
	if( (n0 + n1 + 1 > low) &&
	    !( i & ( 1 << ( n0 + n1 + 1 ) ) ) &&
	    !( ( i & ( 1 << n0 ) ) && ( i & ( 1 << n1 ) ) ) ) {
	  c += ( n0 == n1 ) ? 1 : 2;
	}
      }
	
    anEscapes1[ i ] = c;
  }
}

static int
Escapes1(HalfBoard const board, uint const n)
{
  int af = 0;
    
  for(uint i = 0; i < 12 && i < n; i++ ) {
    if( board[ 24 - n + i ] > 1 ) {
      af |= ( 1 << i );
    }
  }
    
  return anEscapes1[ af ];
}


class Hack {
public:
  Hack(void);
};

Hack::Hack(void)
{
  ComputeTable0();
  ComputeTable1();
}

namespace {
// call table init
Hack h;
}



static void
calculateHalfInputs(HalfBoard const  board,
		    HalfBoard const  boardOpp,
		    float* const     afInput)
{
  int i, j, k, l, nOppBack, n, aHit[ 39 ], nBoard;
    
  /* aanCombination[n] -
     How many ways to hit from a distance of n pips.
     Each number is an index into aIntermediate below. 
  */
  static const int aanCombination[24][5] = {
    {  0, -1, -1, -1, -1 }, /*  1 */
    {  1,  2, -1, -1, -1 }, /*  2 */
    {  3,  4,  5, -1, -1 }, /*  3 */
    {  6,  7,  8,  9, -1 }, /*  4 */
    { 10, 11, 12, -1, -1 }, /*  5 */
    { 13, 14, 15, 16, 17 }, /*  6 */
    { 18, 19, 20, -1, -1 }, /*  7 */
    { 21, 22, 23, 24, -1 }, /*  8 */
    { 25, 26, 27, -1, -1 }, /*  9 */
    { 28, 29, -1, -1, -1 }, /* 10 */
    { 30, -1, -1, -1, -1 }, /* 11 */
    { 31, 32, 33, -1, -1 }, /* 12 */
    { -1, -1, -1, -1, -1 }, /* 13 */
    { -1, -1, -1, -1, -1 }, /* 14 */
    { 34, -1, -1, -1, -1 }, /* 15 */
    { 35, -1, -1, -1, -1 }, /* 16 */
    { -1, -1, -1, -1, -1 }, /* 17 */
    { 36, -1, -1, -1, -1 }, /* 18 */
    { -1, -1, -1, -1, -1 }, /* 19 */
    { 37, -1, -1, -1, -1 }, /* 20 */
    { -1, -1, -1, -1, -1 }, /* 21 */
    { -1, -1, -1, -1, -1 }, /* 22 */
    { -1, -1, -1, -1, -1 }, /* 23 */
    { 38, -1, -1, -1, -1 } /* 24 */
  };
    
  /* One way to hit */ 
  static struct {
    /* if true, all intermediate points (if any) are required;
       if false, one of two intermediate points are required.
       Set to true for a direct hit, but that can be checked with
       nFaces == 1,
    */
    int fAll;

    /* Intermediate points required */
    int anIntermediate[ 3 ];

    /* Number of faces used in hit (1 to 4) */
    int nFaces;

    /* Number of pips used to hit */
    int nPips;
  } *pi,

      /* All ways to hit */
	
      aIntermediate[ 39 ] = {
	{ 1, { 0, 0, 0 }, 1, 1 }, /*  0: 1x hits 1 */
	{ 1, { 0, 0, 0 }, 1, 2 }, /*  1: 2x hits 2 */
	{ 1, { 1, 0, 0 }, 2, 2 }, /*  2: 11 hits 2 */
	{ 1, { 0, 0, 0 }, 1, 3 }, /*  3: 3x hits 3 */
	{ 0, { 1, 2, 0 }, 2, 3 }, /*  4: 21 hits 3 */
	{ 1, { 1, 2, 0 }, 3, 3 }, /*  5: 11 hits 3 */
	{ 1, { 0, 0, 0 }, 1, 4 }, /*  6: 4x hits 4 */
	{ 0, { 1, 3, 0 }, 2, 4 }, /*  7: 31 hits 4 */
	{ 1, { 2, 0, 0 }, 2, 4 }, /*  8: 22 hits 4 */
	{ 1, { 1, 2, 3 }, 4, 4 }, /*  9: 11 hits 4 */
	{ 1, { 0, 0, 0 }, 1, 5 }, /* 10: 5x hits 5 */
	{ 0, { 1, 4, 0 }, 2, 5 }, /* 11: 41 hits 5 */
	{ 0, { 2, 3, 0 }, 2, 5 }, /* 12: 32 hits 5 */
	{ 1, { 0, 0, 0 }, 1, 6 }, /* 13: 6x hits 6 */
	{ 0, { 1, 5, 0 }, 2, 6 }, /* 14: 51 hits 6 */
	{ 0, { 2, 4, 0 }, 2, 6 }, /* 15: 42 hits 6 */
	{ 1, { 3, 0, 0 }, 2, 6 }, /* 16: 33 hits 6 */
	{ 1, { 2, 4, 0 }, 3, 6 }, /* 17: 22 hits 6 */
	{ 0, { 1, 6, 0 }, 2, 7 }, /* 18: 61 hits 7 */
	{ 0, { 2, 5, 0 }, 2, 7 }, /* 19: 52 hits 7 */
	{ 0, { 3, 4, 0 }, 2, 7 }, /* 20: 43 hits 7 */
	{ 0, { 2, 6, 0 }, 2, 8 }, /* 21: 62 hits 8 */
	{ 0, { 3, 5, 0 }, 2, 8 }, /* 22: 53 hits 8 */
	{ 1, { 4, 0, 0 }, 2, 8 }, /* 23: 44 hits 8 */
	{ 1, { 2, 4, 6 }, 4, 8 }, /* 24: 22 hits 8 */
	{ 0, { 3, 6, 0 }, 2, 9 }, /* 25: 63 hits 9 */
	{ 0, { 4, 5, 0 }, 2, 9 }, /* 26: 54 hits 9 */
	{ 1, { 3, 6, 0 }, 3, 9 }, /* 27: 33 hits 9 */
	{ 0, { 4, 6, 0 }, 2, 10 }, /* 28: 64 hits 10 */
	{ 1, { 5, 0, 0 }, 2, 10 }, /* 29: 55 hits 10 */
	{ 0, { 5, 6, 0 }, 2, 11 }, /* 30: 65 hits 11 */
	{ 1, { 6, 0, 0 }, 2, 12 }, /* 31: 66 hits 12 */
	{ 1, { 4, 8, 0 }, 3, 12 }, /* 32: 44 hits 12 */
	{ 1, { 3, 6, 9 }, 4, 12 }, /* 33: 33 hits 12 */
	{ 1, { 5, 10, 0 }, 3, 15 }, /* 34: 55 hits 15 */
	{ 1, { 4, 8, 12 }, 4, 16 }, /* 35: 44 hits 16 */
	{ 1, { 6, 12, 0 }, 3, 18 }, /* 36: 66 hits 18 */
	{ 1, { 5, 10, 15 }, 4, 20 }, /* 37: 55 hits 20 */
	{ 1, { 6, 12, 18 }, 4, 24 }  /* 38: 66 hits 24 */
      };

  /* aaRoll[n] - All ways to hit with the n'th roll
     Each entry is an index into aIntermediate above.
  */
    
  static const int aaRoll[21][4] = {
    {  0,  2,  5,  9 }, /* 11 */
    {  0,  1,  4, -1 }, /* 21 */
    {  1,  8, 17, 24 }, /* 22 */
    {  0,  3,  7, -1 }, /* 31 */
    {  1,  3, 12, -1 }, /* 32 */
    {  3, 16, 27, 33 }, /* 33 */
    {  0,  6, 11, -1 }, /* 41 */
    {  1,  6, 15, -1 }, /* 42 */
    {  3,  6, 20, -1 }, /* 43 */
    {  6, 23, 32, 35 }, /* 44 */
    {  0, 10, 14, -1 }, /* 51 */
    {  1, 10, 19, -1 }, /* 52 */
    {  3, 10, 22, -1 }, /* 53 */
    {  6, 10, 26, -1 }, /* 54 */
    { 10, 29, 34, 37 }, /* 55 */
    {  0, 13, 18, -1 }, /* 61 */
    {  1, 13, 21, -1 }, /* 62 */
    {  3, 13, 25, -1 }, /* 63 */
    {  6, 13, 28, -1 }, /* 64 */
    { 10, 13, 30, -1 }, /* 65 */
    { 13, 31, 36, 38 }  /* 66 */
  };

  /* One roll stat */
  
  struct {
    /* count of pips this roll hits */
    int nPips;
      
    /* number of chequers this roll hits */
    int nChequers;
  } aRoll[ 21 ];
    

  for(nOppBack = 24; nOppBack >= 0; --nOppBack) {
    if( boardOpp[nOppBack] ) {
      break;
    }
  }
    
  nOppBack = 23 - nOppBack;

  n = 0;
  for( i = nOppBack + 1; i < 25; i++ ) {
    if( board[i] ) {
      n += ( i + 1 - nOppBack ) * board[ i ];
    }
  }
  
  {                                                              assert( n ); }
    
  afInput[ I_BREAK_CONTACT ] = n / (15 + 152.0);

  {
    unsigned int p  = 0;
    
    {                       assert(nOppBack == -1 || board[nOppBack] == 0); }
  
    for(int i = 0; i < nOppBack; ++i) {
      if( board[i] )
	p += (i+1) * board[i];
    }
    
    afInput[I_FREEPIP] = p / 100.0;
  }

  {
    int t = 0;
    
    int no = 0;
      
    t += 24 * board[24];
    no += board[24];
      
    for( i = 23;  i >= 12 && i > nOppBack; --i ) {
      if( board[i] && board[i] != 2 ) {
	int n = ((board[i] > 2) ? (board[i] - 2) : 1);
	no += n;
	t += i * n;
      }
    }

    for( ; i >= 6; --i ) {
      if( board[i] ) {
	int n = board[i];
	no += n;
	t += i * n;
      }
    }
    
    for( i = 5;  i >= 0; --i ) {
      if( board[i] > 2 ) {
	t += i * (board[i] - 2);
	no += (board[i] - 2);
      } else if( board[i] < 2 ) {
	int n = (2 - board[i]);

	if( no >= n ) {
	  t -= i * n;
	  no -= n;
	}
      }
    }

    if( t < 0 ) {
      t = 0;
    }

    afInput[ I_TIMING ] = t / 100.0;
  }

  // Back chequer

  {
    int nBack;
    
    for( nBack = 24; nBack >= 0; --nBack ) {
      if( board[nBack] ) {
	break;
      }
    }
    
    afInput[ I_BACK_CHEQUER ] = nBack / 24.0;

    // Back anchor

    for( i = nBack == 24 ? 23 : nBack; i >= 0; --i ) {
      if( board[i] >= 2 ) {
	break;
      }
    }
    
    afInput[ I_BACK_ANCHOR ] = i / 24.0;
    
    /* Forward anchor */

    n = 0;
    for( j = 18; j <= i; ++j ) {
      if( board[j] >= 2 ) {
	n = 24 - j;
	break;
      }
    }

    if( n == 0 ) {
      for( j = 17; j >= 12 ; --j ) {
	if( board[j] >= 2 ) {
	  n = 24 - j;
	  break;
	}
      }
    }
	
    afInput[ I_FORWARD_ANCHOR ] = n == 0 ? 2.0 : n / 6.0;
  }
    

  // Piploss 
    
  nBoard = 0;
  for( i = 0; i < 6; i++ )
    if( board[ i ] )
      nBoard++;

  for( i = 0; i < 39; i++ )
    aHit[ i ] = 0;
    
    /* for every point we'd consider hitting a blot on, */
    
  for( i = ( nBoard > 2 ) ? 23 : 21; i >= 0; i-- )
    /* if there's a blot there, then */
      
    if( boardOpp[ i ] == 1 )
      /* for every point beyond */
	
      for( j = 24 - i; j < 25; j++ )
	/* if we have a hitter and are willing to hit */
	  
	if( board[ j ] && !( j < 6 && board[ j ] == 2 ) )
	  /* for every roll that can hit from that point */
	    
	  for( n = 0; n < 5; n++ ) {
	    if( aanCombination[ j - 24 + i ][ n ] == -1 )
	      break;

	    /* find the intermediate points required to play */
	      
	    pi = aIntermediate + aanCombination[ j - 24 + i ][ n ];

	    if( pi->fAll ) {
	      /* if nFaces is 1, there are no intermediate points */
		
	      if( pi->nFaces > 1 ) {
		/* all the intermediate points are required */
		  
		for( k = 0; k < 3 && pi->anIntermediate[k] > 0; k++ )
		  if( boardOpp[ i - pi->anIntermediate[ k ] ] > 1 )
		    /* point is blocked; look for other hits */
		    goto cannot_hit;
	      }
	    } else {
	      /* either of two points are required */
		
	      if( boardOpp[ i - pi->anIntermediate[ 0 ] ] > 1
		  && boardOpp[ i - pi->anIntermediate[ 1 ] ] > 1 ) {
				/* both are blocked; look for other hits */
		goto cannot_hit;
	      }
	    }
	      
	    /* enter this shot as available */
	      
	    aHit[ aanCombination[ j - 24 + i ][ n ] ] |= 1 << j;
	    cannot_hit: ;
	  }

  for( i = 0; i < 21; i++ )
    aRoll[ i ].nPips = aRoll[ i ].nChequers = 0;
    
  if( !board[ 24 ] ) {
    /* we're not on the bar; for each roll, */
      
    for( i = 0; i < 21; i++ ) {
      n = -1; /* (hitter used) */
	
      /* for each way that roll hits, */
      for( j = 0; j < 4; j++ ) {
	int r = aaRoll[ i ][ j ];
	
	if( r < 0 )
	  break;

	if( !aHit[ r ] )
	  continue;

	pi = aIntermediate + r;
		
	if( pi->nFaces == 1 ) {
	  /* direct shot */
	  for( k = 23; k > 0; k-- ) {
	    if( aHit[ r ] & ( 1 << k ) ) {
	      /* select the most advanced blot; if we still have
		 a chequer that can hit there */
		      
	      if( n != k || board[ k ] > 1 )
		aRoll[ i ].nChequers++;

	      n = k;

	      if( k - pi->nPips + 1 > aRoll[ i ].nPips )
		aRoll[ i ].nPips = k - pi->nPips + 1;
		
	      /* if rolling doubles, check for multiple
		 direct shots */
		      
	      if( aaRoll[ i ][ 3 ] >= 0 &&
		  aHit[ r ] & ~( 1 << k ) )
		aRoll[ i ].nChequers++;
			    
	      break;
	    }
	  }
	} else {
	  /* indirect shot */
	  if( !aRoll[ i ].nChequers )
	    aRoll[ i ].nChequers = 1;

	  /* find the most advanced hitter */
	    
	  for( k = 23; k >= 0; k-- )
	    if( aHit[ r ] & ( 1 << k ) )
	      break;

	  if( k - pi->nPips + 1 > aRoll[ i ].nPips )
	    aRoll[ i ].nPips = k - pi->nPips + 1;

	  /* check for blots hit on intermediate points */
		    
	  for( l = 0; l < 3 && pi->anIntermediate[ l ] > 0; l++ )
	    if( boardOpp[ 23 - k + pi->anIntermediate[ l ] ] == 1 ) {
		
	      aRoll[ i ].nChequers++;
	      break;
	    }
	}
      }
    }
  } else if( board[ 24 ] == 1 ) {
    /* we have one on the bar; for each roll, */
      
    for( i = 0; i < 21; i++ ) {
      n = 0; /* (free to use either die to enter) */
	
      for( j = 0; j < 4; j++ ) {
	int r = aaRoll[ i ][ j ];
	
	if( r < 0 )
	  break;
		
	if( !aHit[ r ] )
	  continue;

	pi = aIntermediate + r;
		
	if( pi->nFaces == 1 ) {
	  /* direct shot */
	  
	  for( k = 24; k > 0; k-- ) {
	    if( aHit[ r ] & ( 1 << k ) ) {
	      /* if we need this die to enter, we can't hit elsewhere */
	      
	      if( n && k != 24 )
		break;
			    
	      /* if this isn't a shot from the bar, the
		 other die must be used to enter */
	      
	      if( k != 24 ) {
		int npip = aIntermediate[aaRoll[ i ][ 1 - j ] ].nPips;
		
		if( boardOpp[npip - 1] > 1 )
		  break;
				
		n = 1;
	      }

	      aRoll[ i ].nChequers++;

	      if( k - pi->nPips + 1 > aRoll[ i ].nPips )
		aRoll[ i ].nPips = k - pi->nPips + 1;
	    }
	  }
	} else {
	  /* indirect shot -- consider from the bar only */
	  if( !( aHit[ r ] & ( 1 << 24 ) ) )
	    continue;
		    
	  if( !aRoll[ i ].nChequers )
	    aRoll[ i ].nChequers = 1;
		    
	  if( 25 - pi->nPips > aRoll[ i ].nPips )
	    aRoll[ i ].nPips = 25 - pi->nPips;
		    
	  /* check for blots hit on intermediate points */
	  for( k = 0; k < 3 && pi->anIntermediate[ k ] > 0; k++ )
	    if( boardOpp[ pi->anIntermediate[ k ] + 1 ] == 1 ) {
		
	      aRoll[ i ].nChequers++;
	      break;
	    }
	}
      }
    }
  } else {
    /* we have more than one on the bar --
       count only direct shots from point 24 */
      
    for( i = 0; i < 21; i++ ) {
      /* for the first two ways that hit from the bar */
	
      for( j = 0; j < 2; j++ ) {
	int r = aaRoll[ i ][ j ];
	
	if( !( aHit[r] & ( 1 << 24 ) ) )
	  continue;

	pi = aIntermediate + r;

	/* only consider direct shots */
	
	if( pi->nFaces != 1 )
	  continue;

	aRoll[ i ].nChequers++;

	if( 25 - pi->nPips > aRoll[ i ].nPips )
	  aRoll[ i ].nPips = 25 - pi->nPips;
      }
    }
  }

  {
    int np = 0;
    int n1 = 0;
    int n2 = 0;
      
    for(i = 0; i < 21; i++) {
      int w = aaRoll[i][3] > 0 ? 1 : 2;
      int nc = aRoll[i].nChequers;
	
      np += aRoll[i].nPips * w;
	
      if( nc > 0 ) {
	n1 += w;

	if( nc > 1 ) {
	  n2 += w;
	}
      }
    }

    afInput[ I_PIPLOSS ] = np / ( 12.0 * 36.0 );
      
    afInput[ I_P1 ] = n1 / 36.0;
    afInput[ I_P2 ] = n2 / 36.0;
  }

  afInput[ I_BACKESCAPES ] = Escapes( board, 23 - nOppBack ) / 36.0;

  afInput[ I_BACKRESCAPES ] = Escapes1( board, 23 - nOppBack ) / 36.0;

  for( n = 36, i = 15; i < 24 - nOppBack; i++ )
    if( ( j = Escapes( board, i ) ) < n )
      n = j;

  afInput[ I_ACONTAIN ] = ( 36 - n ) / 36.0;
  afInput[ I_ACONTAIN2 ] = afInput[ I_ACONTAIN ] * afInput[ I_ACONTAIN ];

  if( nOppBack < 0 ) {
    /* restart loop, point 24 should not be included */
    i = 15;
    n = 36;
  }
    
  for( ; i < 24; i++ )
    if( ( j = Escapes( board, i ) ) < n )
      n = j;

    
  afInput[ I_CONTAIN ] = ( 36 - n ) / 36.0;
  afInput[ I_CONTAIN2 ] = afInput[ I_CONTAIN ] * afInput[ I_CONTAIN ];

    
  for(n = 0, i = 6; i < 25; ++i) {
    if( board[i] )
      n += ( i - 5 ) * board[ i ] * Escapes( boardOpp, i );
  }
  
  afInput[ I_MOBILITY ] = n / 3600.00;

  j = 0;
  n = 0; 
  for(i = 0; i < 25; i++ ) {
    int ni = board[ i ];
      
    if( ni ) {
      j += ni;
      n += i * ni;
    }
  }

  if( j ) {
    n = (n + j - 1) / j;
  }

  j = 0;
  for(k = 0, i = n + 1; i < 25; i++ ) {
    int ni = board[ i ];

    if( ni ) {
      j += ni;
      k += ni * ( i - n ) * ( i - n );
    }
  }

  if( j ) {
    k = (k + j - 1) / j;
  }

  afInput[ I_MOMENT2 ] = k / 400.0;

  if( board[ 24 ] > 0 ) {
    int loss = 0;
    int two = board[ 24 ] > 1;
      
    for(i = 0; i < 6; ++i) {
      if( boardOpp[ i ] > 1 ) {
	/* any double loses */
	  
	loss += 4*(i+1);

	for(j = i+1; j < 6; ++j) {
	  if( boardOpp[ j ] > 1 ) {
	    loss += 2*(i+j+2);
	  } else {
	    if( two ) {
	      loss += 2*(i+1);
	    }
	  }
	}
      } else {
	if( two ) {
	  for(j = i+1; j < 6; ++j) {
	    if( boardOpp[ j ] > 1 ) {
	      loss += 2*(j+1);
	    }
	  }
	}
      }
    }
      
    afInput[ I_ENTER ] = loss / (36.0 * (49.0/6.0));
  } else {
    afInput[ I_ENTER ] = 0.0;
  }

  n = 0;
  for(i = 0; i < 6; i++ ) {
    n += boardOpp[ i ] > 1;
  }
    
  afInput[ I_ENTER2 ] = ( 36 - ( n - 6 ) * ( n - 6 ) ) / 36.0; 

  {
    int pa = -1;
    int w = 0;
    int tot = 0;
    
    for(int np = 23; np > 0; --np) {
      if( board[np] >= 2 ) {
	if( pa == -1 ) {
	  pa = np;
	  continue;
	}

	int d = pa - np;
	int c = 0;
	
	if( d <= 6 ) {
	  c = 11;
	} else if( d <= 11 ) {
	  c = 13 - d;
	}

	w += c * board[pa];
	tot += board[pa];
      }
    }

    if( tot ) {
      afInput[I_BACKBONE] = 1 - (w / (tot * 11.0));
    } else {
      afInput[I_BACKBONE] = 0;
    }
  }

  {
    unsigned int nAc = 0;
    
    for( i = 18; i < 24; ++i ) {
      if( board[i] > 1 ) {
	++nAc;
      }
    }
    
    afInput[I_BACKG] = 0.0;
    afInput[I_BACKG1] = 0.0;

    if( nAc >= 1 ) {
      unsigned int tot = 0;
      for( i = 18; i < 25; ++i ) {
	tot += board[i];
      }

      if( nAc > 1 ) {
	assert( tot >= 4 );
      
	afInput[I_BACKG] = (tot - 3) / 4.0;
      } else if( nAc == 1 ) {
	afInput[I_BACKG1] = tot / 8.0;
      }
    }
  }
}


static void
menOffAll(const uint* const board, float* const inputs)
{
  int menOff = 15;
  
  for(uint i = 0; i < 25; i++ ) {
    menOff -= board[i];
  }

  if( menOff > 10 ) {
    inputs[ 0 ] = 1.0;
    inputs[ 1 ] = 1.0;
    inputs[ 2 ] = ( menOff - 10 ) / 5.0;
  } else if( menOff > 5 ) {
    inputs[ 0 ] = 1.0;
    inputs[ 1 ] = ( menOff - 5 ) / 5.0;
    inputs[ 2 ] = 0.0;
  } else {
    inputs[ 0 ] = menOff ? menOff / 5.0 : 0.0;
    inputs[ 1 ] = 0.0;
    inputs[ 2 ] = 0.0;
  }
}

static void
menOffNonCrashed(const uint* const board, float* const inputs)
{
  int menOff = 15;
  
  for(uint i = 0; i < 25; ++i) {
    menOff -= board[i];
  }
  {                                                   assert( menOff <= 8 ); }
    
  if( menOff > 5 ) {
    inputs[ 0 ] = 1.0;
    inputs[ 1 ] = 1.0;
    inputs[ 2 ] = ( menOff - 6 ) / 3.0;
  } else if( menOff > 2 ) {
    inputs[ 0 ] = 1.0;
    inputs[ 1 ] = ( menOff - 3 ) / 3.0;
    inputs[ 2 ] = 0.0;
  } else {
    inputs[ 0 ] = menOff ? menOff / 3.0 : 0.0;
    inputs[ 1 ] = 0.0;
    inputs[ 2 ] = 0.0;
  }
}

void
contactGetInputs(Board const& b, float* const inputs)
{
  basicContactInputs(b, inputs);

  {
    float* const i = inputs + 4 * 25 * 2;

    /* I accidentally switched sides (0 and 1) when I trained the net */
    menOffNonCrashed(b[0], i + I_OFF1);
    calculateHalfInputs(b[1], b[0], i);
  }

  {
     float* const i = inputs + (4 * 25 * 2 + HALF_INPUTS);

     menOffNonCrashed(b[1], i + I_OFF1);
     calculateHalfInputs(b[0], b[1], i);
  }
}

void
crashedGetInputs(Board const& b, float* const inputs)
{
  basicContactInputs(b, inputs);

  {
    float* const i = inputs + 4 * 25 * 2;

    /* I accidentally switched sides (0 and 1) when I trained the net */
    menOffAll(b[1], i + I_OFF1);
    calculateHalfInputs(b[1], b[0], i);
  }

  {
     float* const i = inputs + (4 * 25 * 2 + HALF_INPUTS);

     menOffAll(b[0], i + I_OFF1);
     calculateHalfInputs(b[0], b[1], i);
  }
}

const NeuralNet* bearoffGetNet(void)
{
    return 0;
}

const NeuralNet* raceGetNet(void)
{
    return 0;
}

const NeuralNet* contactGetNet(void)
{
    return 0;
}

const NeuralNet* crashedGetNet(void)
{
    return 0;
}