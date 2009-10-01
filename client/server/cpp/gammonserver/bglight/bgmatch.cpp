#if defined( __GNUG__ )
#pragma implementation
#endif

/*
 * bgmatch.cc
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

/*@top BGcore Library

  BGcore is @value{product} main library. Every player consists of
  @ref{../interfaces,user interface code} linked with bgcore.

  BGcore provides most of the specialized knowledge, while the UI side needs
  to know just a little backgammon to do it's part.

  The library only @dfn{entry point} is the @ref{Match} class. UI code creates
  one Match object for every match. See @ref{User Interface Interaction} for a
  detailed overview of the interface */


/*@chapter User Interface Interaction

  After creating a @ref{Match} @arg{m}, UI follows a relatively simple loop,

@enumerate
@item
Get @ref{Match::next,events} from @arg{m} and present them to user one at a
time.
@item Collect one user action.
@item @ref{User response calls,Pass} user action to @arg{m}.
@item Go back to step 1.
@end

Collecting a user action can be as simple as reporting a button press, or
slightly more complex such as collecting a move.


Below is a rough sketch of user code flow. It is written in pseudocode and
omits some details, but is useful for understanding the overall structure. For
a real example have a look at @file{../interfaces/tkbglight/tklight.tcl}.

For lack of better names I shall call the two sides Hal (computer) and Dave
(human).

@example

Create @arg{m}, display match score, cube, and initial board.

LOOP until match is over
  Call actions (see below)

  IF Dave turn and he may double
     wait for Dave to either
       double  - and call @ref{Match::opDoubles}
       or roll - and call @ref{Match::opRolls,opRolls}
  ELSEIF Dave just rolled
    collect a move, then call @ref{Match::opPlays,opPlays}

END-LOOP
@end

Notes:
@itemize
@item
I have omitted handling of Dave resignations, handled by calling
@ref{Match::opResigns}.
@item
Real code will probably dispense with the @code{IF} statement by setting up
the appropriate buttons/pointing device (as part of the actions code),
then simply wait for Dave action.
@end

Here is the @code{actions} code,

@example

while ( unprocessed event @ref{Match::next,@arg{e}} ) {
  switch ( @ref{Action::Type,type(@arg{e})} ) {
    ROLLS:
      IF Hal rolled  - Draw dice on Hal side of the board@footnote{this is the
custom in backgammon}.
      IF Dave rolled - Draw dice on Hal side of the board.

    MOVES:
      Move checkers on board.
      
      IF Hal moved
        IF Dave may double
	  Enable double button
        ELSE
	  // Nothing to do. The next action will be a roll for Dave.

      ELSE	
        // Dave moved. We get this only when automoves are enabled and this
	// was a forced move
	Pause for a short time to let Dave see what is happening.
      	
    DOUBLES:
      // This can only be Hal. Dave doubles are not reported back.

      Draw doubling cube inside the board on Dave side of board.
      Open a dialog box informing Dave he has been doubled, and ask if he
      takes or drops.
      IF Dave accepts
        Notify Hal (call @ref{Match::opTakes,opTakes})
        Draw doubling cube on board margins, on Dave side of the board
      ELSE
        Notify Hal (call @ref{Match::opTakes,opDrops})

    TAKES:
      // This can only be Hal. Dave takes are not reported back.
      Draw doubling cube on board margins, on Hal side of the board

    DROPS:
      // This can only be Hal. Dave drops are not reported back.
      Notify Dave that he won this game
      
    RESIGNS:
      // This can only be Hal. Dave drops are not reported back.
      Inform Dave that Hal is resigning and how many points he will get. Call
      @ref{Match::opAnswers,opAccepts}(true) if Dave agrees,opAnswers(false)@
 otherwise.

    WINS:
      Notify Dave who won and how many points.
      Set up for a new game - update display of scores, doubling cube, board
      and so on.

    CRAWFORD:
      Don't let Dave double this game, as this is the Crawford game.
  }
}

@end
*/



#ifdef HAVE_CONFIG_H
#include <config.h>
#endif

#include <iostream>

#if HAVE_SSTREAM
#include <sstream>
#else
#include <strstream>
#endif

#include <iomanip>

#include "debug.h"
#include "stdutil.h"

#include "bgmatch.h"
#include "equities.h"
#include "moves.h"
#include "bg.h"
#include "rand.h"

using std::vector;

#ifdef WIN32
#include <windows.h>
#else
using std::min;
using std::max;
#endif

using std::ostream;
using std::istream;
using std::endl;
using std::ws;

#if HAVE_SSTREAM
typedef std::ostringstream ost;

static inline uint
length(ost const& s)
{
  return s.str().length();
}

#else

typedef std::ostrstream ost;

static inline uint
length(ost& s)
{
  // Assume it is called after adding the ends, so stream contains one more
  // than string.
  
  return s.pcount() - 1;
}

#endif

using std::ends;
using std::setw;

namespace BG {

Settings::Settings(void) :
  autoForcedMoves(false),
  level(EXPERT)
{}

void
Settings::setRandSeed(unsigned long const s)
{
  Random::setSeed(s);
}

Settings settings;
namespace {
  // (FIXME) I need to work out the actual values

  float const levelNoise[3] = { 0.03, 0.01, 0.0 };
}


Match::Action::Action(Type const t, Side const s) :
  actionType(t),
  side(s)
{}

bool
Match::Action::putSGF(ostream& to) const
{
  switch( actionType ) {
    case ROLLS:
    {
      to << "PL[" << ((side == X) ? 'w' : 'b') << "]DI["
	 << dice[0] << dice[1] << "]" << endl;
      break;
    }
    case MOVES:
    {
      to << ((side == X) ? 'W' : 'B') << "[";
      for(uint k = 0; k < 8 && move.desc[k] >= 0; ++k) {
	switch( int const n = move.desc[k] ) {
	  case 25: { to << 'y'; break; }
	  case  0: { to << 'z'; break; }
	  default: { to << char((side == X) ? 'a' + n : 'x' - n); }
	}
      }
      to << "]" << endl;
      break;
    }
    case DOUBLES:
    {
      to << ((side == X) ? 'W' : 'B') << "[double]" << endl;
      break;
    }
    case TAKES:
    {
      to << ((side == X) ? 'W' : 'B') << "[take]" << endl;
      break;
    }
    case DROPS:
    {
      to << ((side == X) ? 'W' : 'B') << "[drop]" << endl;
      break;
    }
    case REJECTS:
    {
      to << ((side == X) ? 'W' : 'B') << "[rejects]" << endl;
      break;
    }
    case RESIGNS:
    {
      to << ((side == X) ? 'W' : 'B') << "[resigns]" << endl;
      break;
    }
    case WINS:
    {
      bool resigns = false;
      to << "RE[" << ((side == X) ? 'W' : 'B') << "+" << points <<
	(resigns ? "R" : "") << "]" << endl;
      return true;
    }
    case CRAWFORD:
    {
      to << "RU[Crawford:CrawfordGame]";
    }
  }
  return false;
}

bool
Match::Action::put(ostream& to) const
{
  switch( actionType ) {
    case ROLLS:
    {
      to << endl << "DI " << ((side == X) ? 'X' : 'O') << " "
	 << dice[0] << " " << dice[1];
      break;
    }
    case MOVES:
    {
      to << endl << "MO " << ((side == X) ? 'X' : 'O');
      uint k = 0;
      for(/**/; k < 8 && move.desc[k] >= 0; ++k) {
	to << " " << move.desc[k];
      }
      if( k < 8 ) {
	to << " -1";
      }
      break;
    }
    case DOUBLES:
    {
      to << endl << "DO " << ((side == X) ? 'X' : 'O') << " " << points;
      break;
    }
    case TAKES:
    {
      to << endl << "TA " << ((side == X) ? 'X' : 'O');
      break;
    }
    case DROPS:
    {
      to << endl << "DR " << ((side == X) ? 'X' : 'O');
      break;
    }
    case REJECTS:
    {
      to << endl << "RJ " << ((side == X) ? 'X' : 'O');
      break;
    }
    case RESIGNS:
    {
      to << endl << "RE " << ((side == X) ? 'X' : 'O') << " " << points;
      break;
    }
    case WINS:
    {
      to << endl << "WI " << ((side == X) ? 'X' : 'O') << " " << points;
      return true;
    }
    case CRAWFORD:
    {
      to << endl << "CR H";
    }
  }
  return false;
}

Match::Action::Action(istream& from)
{
  char type[2];
  char sd;
  
  from >> type[0] >> type[1] >> sd;

  side = (sd == 'X') ? X : O;
  
  switch( type[0] ) {
    case 'D':
    {
      switch( type[1] ) {
	case 'O':
	{
	  actionType = DOUBLES;
	  from >> points;
	  break;
	}
	case 'I':
	{
	  actionType = ROLLS;
	  from >> dice[0] >> dice[1];
	  break;
	}
	case 'R':
	{
	  actionType = DROPS;
	  break;
	}
	default:
	{
#if defined( HAVE_EXCEPTIONS )
	  throw;
#else
	  actionType = static_cast<Type>(-1);
	  return;
#endif
	}
      }
      break;
    }
    case 'M':
    {
      actionType = MOVES;
      for(uint k = 0; k < 8; ++k) {
	from >> move.desc[k];
	if( move.desc[k] < 0 ) {
	  break;
	}
      }
      break;
    }
    case 'T':
    {
      actionType = TAKES;
      break;
    }
    case 'R':
    {
      switch( type[1] ) {
	case 'J':
	{
	  actionType = REJECTS;
	  break;
	}
	case 'E':
	{
	  actionType = RESIGNS;
	  from >> points;
	  break;
	}
      }
      break;
    }
    case 'W':
    {
      actionType = WINS;
      from >> points;
      break;
    }
    case 'C':
    {
      actionType = CRAWFORD;
      break;
    }
    default:
    {
#if defined( HAVE_EXCEPTIONS )
      throw;
#else
      actionType = static_cast<Type>(-1);
      return;
#endif
    }
  }
  
  from >> ws;
}


Match::Match(uint const matchLength_, const char* const opponentName) :
  bg(*(new BackGammon)),
  matchLength(matchLength_),
  xScore(0),
  oScore(0),
  crawfordDone(false),
  nextAction(0)
{
  startGame();

  stat.computerName = strdup(applicationName);
  stat.opponentName = strdup(opponentName ? opponentName : "You");
}



const Match::Action*
Match::next(void)
{
  if( nextAction < matchRecord.size() ) {
    const Action* a = matchRecord[nextAction];
      
    while( a->actionType == a->RESIGNS && a->side == X ) {
      {        assert( matchRecord[nextAction+1]->actionType == a->REJECTS ); }
      
      nextAction += 2;

      if( nextAction == matchRecord.size() ) {
	return 0;
      }
      a = matchRecord[nextAction];
    }

    ++nextAction;
    
    return a;
  }
  return 0;
}

Match::Action const&
Match::last(void) const
{
  return **(matchRecord.end() - 1);
}
  

void
Match::startGame(void)
{
  if( xScore >= matchLength || oScore >= matchLength ) {
    xScore = min(xScore, matchLength);
    oScore = min(oScore, matchLength);
    
    // match over
    return;
  }

  bool const finishedCrawfordGame = bg.state().crawfordGame;
  
  bg.setScore(matchLength - xScore, matchLength - oScore);

  if( crawfordDone || finishedCrawfordGame ) {
    crawfordDone = true;
  } else {
    if( xScore + 1 == matchLength || oScore + 1 == matchLength ) {
      bg.crawford(true);
      Action* c = new Action(c->CRAWFORD, X);
      matchRecord.insert(matchRecord.end(), c);
    }
  }
  
  currentBoard.initial();

  Action* a = new Action(Action::ROLLS, X);
  int* dice = a->dice;
  
  dice[0] = dice[1] = 0;
  while( dice[0] == dice[1] ) {
    roll(dice);
  }
  
  a->side = (dice[0] > dice[1]) ? X : O;

  matchRecord.insert(matchRecord.end(), a);

#ifdef ENABLE_BOT
  if( a->side == O ) {
    doMove(*a);
  }
#endif

}

inline bool
Match::opTurnOK(Action const& prev)
{
  return ((prev.side == O &&
	   (prev.actionType == prev.MOVES ||  prev.actionType == prev.TAKES))
	  || (prev.actionType == prev.REJECTS));
}

bool
Match::opRolls(void)
{
  Action const& p = last();

  if( ! opTurnOK(p) ) {
    return false;
  }

  Action* a = new Action(a->ROLLS, X);
  roll(a->dice);

  matchRecord.insert(matchRecord.end(), a);

  if( settings.autoForcedMoves ) {
    MoveDesc d;
    if( bg.autoMove(currentBoard, a->dice, 0, d) ) {
      // save nextAction and restore later. opPlays sets it so that
      // opponent moves are not sent back.
      
      uint const l = nextAction;

      opPlays(d);
      
      nextAction = l;
    }
  }

  return true;
}



bool
Match::opDoubles(void)
{
  Action const& p = last();

  if( ! opTurnOK(p) ) {
    return false;
  }

  {
    MatchState const& s = bg.state();
    if( s.crawfordGame || (s.cube > 1 && ! s.xOwns) ) {
      return false;
    }
  }
  
  {                               assert( nextAction == matchRecord.size() ); }
  Action* d = new Action(Action::DOUBLES, X);
  d->points = bg.state().cube * 2;
  matchRecord.insert(matchRecord.end(), d);

  nextAction = matchRecord.size();

  Board const opBoard(currentBoard, true);
  bool const accept = bg.acceptDouble(opBoard, true);

  Action* a = new Action(accept ? a->TAKES : a->DROPS, O);
  matchRecord.insert(matchRecord.end(), a);

  if( accept ) {
    bg.setCube(2 * bg.state().cube, false);
#if !defined( NDEBUG )
    bool const ok =
#endif
      opRolls();                                                    assert(ok);
  } else {
    Action* w = new Action(w->WINS, X);
    w->points = bg.state().cube;
    matchRecord.insert(matchRecord.end(), w);

    xScore += w->points;
    startGame();
  }

  return true;
}

bool
Match::partialMoveLegal(MoveDesc const& move) const
{
  Action const& r = last();
  
  if( ! ( r.actionType == r.ROLLS && r.side == X ) ) {
    return false;
  }

  return bg.partialMoveLegal(currentBoard, r.dice, move, 0);
}

bool
Match::doMove(Action const& r, MoveDesc const& move)
{
  Action* m = new Action(m->MOVES, O);

  m->move = move;

#ifdef ENABLE_BOT
  uint resignFactor;
  bg.findBestMove(m->move, r.dice, currentBoard, false, &resignFactor,
		  levelNoise[settings.level], 0);
#else
   if( ! bg.legal(currentBoard, r.dice, move, 1) ) {
    return false;
  }
#endif

#ifdef ENABLE_BOT
  currentBoard.swapSides();
#endif

  matchRecord.insert(matchRecord.end(), m);

  if( int const np = bg.gameOver(currentBoard) ) {
    Action* w = new Action(w->WINS, O);
    w->points = bg.state().cube * abs(np);
    matchRecord.insert(matchRecord.end(), w);

    oScore += w->points;
    startGame();
  }
  else {
#ifdef ENABLE_BOT
    if( resignFactor > 0 ) 
    {
      Action* a = new Action(a->RESIGNS, O);
      a->points = resignFactor;
      matchRecord.insert(matchRecord.end(), a);
    }
    else 
#endif
    {
      // If side can't (or have no reason to), double, roll for him
      if( ! bg.mayDouble(true) ) {
	ICC(bool const ok =) opRolls();                             assert(ok);
      }
    }
  }

  return true;
}

void
Match::rollAndPlay(void)
{
  Action* r = new Action(r->ROLLS, O);
  roll(r->dice);
  matchRecord.insert(matchRecord.end(), r);

#ifdef ENABLE_BOT
  doMove(*r);
#endif
}

Match::Action const&
Match::lastNoneRejectAction(void) const
{
#ifdef WIN32  
    TVecAction::
#else  
    vector<Action*>::
#endif
  const_iterator r = matchRecord.end() - 1;


  // skip resignations, if any
  while( (*r)->actionType == (*r)->REJECTS && (*r)->side == O ) {
    r -= 2;
  }

  return **r;
}


bool
Match::opPlays(MoveDesc const& move)
{
  Action const& r = lastNoneRejectAction();
  
  // This could happen only if there is a bug in the UI

  if( ! ( r.actionType == r.ROLLS && r.side == X ) ) {
    return false;
  }

  if( ! bg.legal(currentBoard, r.dice, move, 0) ) {
    return false;
  }
  
  {
    Action* m = new Action(m->MOVES, X);
    m->move = move;
    matchRecord.insert(matchRecord.end(), m);
    nextAction = matchRecord.size();
  }

  if( int const np = bg.gameOver(currentBoard) ) {
     Action* w = new Action(w->WINS, X);
     w->points = bg.state().cube * abs(np);
     matchRecord.insert(matchRecord.end(), w);

     xScore += w->points;
     startGame();
     
  } else {
    if( bg.offerDouble(currentBoard, false) ) {
      Action* d = new Action(Action::DOUBLES, O);
      d->points = bg.state().cube * 2;
      matchRecord.insert(matchRecord.end(), d);
    } else {
      rollAndPlay();
    }
  }

  return true;
}

bool
Match::opTakes(void)
{
  Action const& r = last();

  if( ! (r.actionType == r.DOUBLES && r.side == O) ) {
    // action out of turn
    return false;
  }

  Action* a = new Action(a->TAKES, X);
  matchRecord.insert(matchRecord.end(), a);

  bg.setCube(2 * bg.state().cube, true);
  
  rollAndPlay();

  return true;
}


bool
Match::opDrops(void)
{
  Action const& r = last();

  if( ! (r.actionType == r.DOUBLES && r.side == O) ) {
    // action out of turn
    return false;
  }

  Action* a = new Action(a->DROPS, X);
  matchRecord.insert(matchRecord.end(), a);
  nextAction = matchRecord.size();
  
  Action* w = new Action(w->WINS, O);
  w->points = bg.state().cube;
  matchRecord.insert(matchRecord.end(), w);

  oScore += w->points;
  startGame();

  return true;
}

bool
Match::opAnswers(bool const accept)
{
  Action const& r = last();

  if( ! ( r.actionType == r.RESIGNS && r.side == O ) ) {
    // action out of turn
    return false;
  }
  
  if( accept ) {
    Action* w = new Action(w->WINS, X);
    w->points = r.points * bg.state().cube;
    matchRecord.insert(matchRecord.end(), w);

    xScore += w->points;
    startGame();
  } else {
    Action* r = new Action(r->REJECTS, X);
    matchRecord.insert(matchRecord.end(), r);
    nextAction = matchRecord.size();

    if( ! bg.mayDouble(true) ) {
      ICC(bool const ok =)
      opRolls();                                                   assert(ok);
    }
  }
  
  return true;
} 
    

void
Match::opResigns(uint factor)
{
  factor = max(min(factor, 3U), 1U);

  bool accept = true;
  uint const cube = bg.state().cube;
  
  if( factor < 3 && oScore + factor * cube < matchLength ) {
    Action const& p = lastNoneRejectAction();

    if( p.actionType == p.MOVES && p.side == O ) {
      // resigns after my move

      Board b(currentBoard, true);
      
      accept = bg.resignsBeforeRoll(b, factor, true);
    }
    else if( p.actionType == p.ROLLS && p.side == X ) {
      // resigns after rolling

#ifdef ENABLE_BOT
      Board b(currentBoard, true);
      MoveDesc m;
      bg.findBestMove(m, p.dice, b, true, 0);

      accept = bg.resigns(b, factor, true);
#else
      accept = false;
#endif
    } else {
      accept = false;
    }
  }

  Action* a = new Action(a->RESIGNS, X);
  a->points = factor;
  matchRecord.insert(matchRecord.end(), a);
  nextAction = matchRecord.size();

  if( accept ) {
    Action* w = new Action(w->WINS, O);
    w->points = cube * factor;
    matchRecord.insert(matchRecord.end(), w);

    oScore += w->points;
    startGame();
  } else {
    Action* r = new Action(r->REJECTS, O);
    matchRecord.insert(matchRecord.end(), r);
  }
}

void
Match::roll(int dice[2]) const
{
  dice[0] = ( Random::get() % 6 ) + 1;
  dice[1] = ( Random::get() % 6 ) + 1;
}


bool
Match::applyMove(Board& b, MoveDesc const& move, uint const side)
{
  return MoveGenerator::play(b, move, side);
}
  

void
Match::exportSGF(ostream& to)
{
  uint nGame = 1;
  uint xs = 0;
  uint os = 0;
  
  // fixed header
  to << "(;FF[4]GM[6]AP[" << applicationName << ":" << version << "]";

  to << "MI[length:" << matchLength << "]" << endl;

  // x == w, o == b

  to << "PW[" << stat.opponentName
     << "]PB[" << stat.computerName << "]";
  
  to << "[game:" << nGame << "][ws:" << xs << "][bs:" << os << "]" << endl;

  for(
#ifdef WIN32  
    TVecAction::
#else  
    vector<Action*>::
#endif
    const_iterator ia = matchRecord.begin();
      ia < matchRecord.end(); ++ia) {
    Action const& a = **ia;

    if( a.putSGF(to) ) {
      if( ia + 1 < matchRecord.end() ) {
	++nGame;
	uint& s = (a.side == X) ? xs : os;
	s += a.points;
	
	to << "[game:" << nGame << "][ws:" << xs << "][bs:" << os << "]"
	   << endl;
      }
    }
  }

  to << ")";
}

void
Match::save(ostream& to) const
{
  uint nGame = 1;
  uint xs = 0;
  uint os = 0;
  
  // fixed header
  to << applicationName << " " << version << endl
     << "ML " << matchLength << endl
     << "NA X " << stat.opponentName << endl
     << "NA O " << stat.computerName << endl
     << "GA " << nGame << endl
     << "SC X 0" << endl
     << "SC O 0";

  for(
#ifdef WIN32  
    TVecAction::
#else  
    vector<Action*>::
#endif
    const_iterator ia = matchRecord.begin();
      ia < matchRecord.end(); ++ia) {
    Action const& a = **ia;

    if( a.put(to) ) {
      if( ia + 1 < matchRecord.end() ) {
	++nGame;
	uint& s = (a.side == X) ? xs : os;
	s += a.points;
	
	to << endl << "GA " << nGame << endl
	   << "SC X " << xs << endl
	   << "SC O " << os;
      }
    }
  }

  to << endl;
}

static inline void
outputAndReset(ostream&    to,
	       ost&        col1,
	       ost&        col2,
	       uint const  target,
	       uint const  pre)
{
#if ! HAVE_SSTREAM
  col1 << ends; col2 << ends;
#endif

  // pad =  target col - 1 blank written - (pre + first column)
  uint const pad = target - (pre  + length(col1));
  
  to << col1.str()
     << setw(pad) << " "
     << col2.str() << endl;
      
#if HAVE_SSTREAM
  col1.str(""); col2.str("");
#else
  col1.seekp(0, std::ios::beg); col1.freeze(0);
  col2.seekp(0, std::ios::beg); col2.freeze(0);
#endif
}
  
void
Match::exportMAT(ostream& to) const
{
  to << "Match 1: " << matchLength << " point match" << endl;

  //uint const icol1 = 1;
  uint const icol2 = 34;

  ost col1, col2;
  uint nGame = 1;
  uint nMove = 0;
  uint xScore = 0;
  uint oScore = 0;
  bool gameStart = true;
  
  for(
#ifdef WIN32  
    TVecAction::
#else  
    vector<Action*>::
#endif
      const_iterator ia = matchRecord.begin();
      ia < matchRecord.end(); ++ia) {
    Action const& a = **ia;

    if( gameStart ) {
      to << endl << " Game " << nGame << endl;
      col1 << stat.opponentName << " : " << xScore;
      col2 << stat.computerName << " : " << oScore;

      to << " ";
      outputAndReset(to, col1, col2, icol2, 1);
      
      ++nGame;
      nMove = 1;
      gameStart = false;
    }

    ost& c = (a.side == X) ? col1 : col2;
    
    switch( a.actionType ) {
      case Action::ROLLS:
      {
	c << a.dice[0] << a.dice[1] << ":";
	continue;
	break;
      }
      case Action::MOVES:
      {
	for(const int* m = a.move.desc;
	    m < a.move.desc + 8 && *m >= 0; m += 2) {
	  c << " " << m[0] << "/" << m[1];
	}
	break;
      }
      case Action::DOUBLES:
      {
	c << " Doubles => " << a.points;
	break;
      }
      case Action::TAKES:
      {
	c << " Takes";
	break;
      }
      case Action::DROPS:
      {
	c << " Drops";
	break;
      }
      case Action::WINS:
      {
	c << " Wins " << a.points << " point" << ((a.points > 1) ? "s" : "");

	uint& s = a.side == X ? xScore : oScore;
	s += a.points;
	
	if( s >= matchLength ) {
	  c << " and the match";
	}
	
	if( a.side == X ) {
	  to << "     ";
	  outputAndReset(to, col1, col2, icol2, 5);
	}
	gameStart = true;
	break;
      }
      case Action::CRAWFORD:
      {
	continue;
      }
      case Action::RESIGNS:
      case Action::REJECTS:
      {
	continue;
      }
    }

    if( a.side == O ) {
      uint pad = 0;
      if( ! (a.actionType == a.WINS && length(col1) == 0) ) {
	to << " " << setw(2) << nMove << ") ";
	pad = 5;
	++nMove;
      }
    
      outputAndReset(to, col1, col2, icol2, pad);
    }
  }
}

void
Match::clearRecord(void)
{
  for(
#ifdef WIN32  
    TVecAction::
#else  
    vector<Action*>::
#endif
      iterator i = matchRecord.begin();
      i < matchRecord.end(); ++i) {
    delete *i;
  }
}  

void my_assert(bool er)
{
    if (!er)
        std::cerr << "my_assert: error!!!";
}

Match::Match(istream& from) :
  bg(*(new BackGammon)),
  crawfordDone(false)
{
  char line[256];
  char t[2];
  char side;

  from.getline(line, sizeof(line));
  from >> t[0] >> t[1] >> matchLength >> ws;

  
#ifdef GMS_DEBUG
  std::cerr << "Match::Match line: " << line;
#endif

  from.getline(line, sizeof(line));

  {               my_assert( from.good() ); assert( strneq(line, "NA X ", 5) ); }

  stat.opponentName = strdup(line + 5);

  from.getline(line, sizeof(line)); assert( from.good() );

  {                                      assert( strneq(line, "NA O ", 5) ); }

  stat.computerName = strdup(line + 5);

  // Game line
  from.getline(line, sizeof(line));                      assert( from.good() );

  {                                            assert( streq(line, "GA 1") ); }

  from >> t[0] >> t[1] >> side >> xScore >> ws;          assert( from.good() );
  from >> t[0] >> t[1] >> side >> oScore >> ws;          assert( from.good() );

  bool crawfordSeen = false;
  
  while( from.good() && ! from.eof() ) {
    Action* a = new Action(from);

#if ! defined( HAVE_EXCEPTIONS )
    if( a->actionType == -1 ) {
      // No exceptions. file corrupted, just go out of loop
      break;
    }
#endif

    matchRecord.insert(matchRecord.end(),a);

    switch( a->actionType ) {
      case Action::WINS:
      {
	(a->side == X ? xScore : oScore) += a->points;

	// try to read start of new game.
	from.getline(line, sizeof(line));
	
	if( line[0] == 'G' && line[1] == 'A' ) {
	  from >> t[0] >> t[1] >> side >> xScore >> ws; assert( from.good() );
	  from >> t[0] >> t[1] >> side >> oScore >> ws; assert( from.good() );
	}

	if( crawfordSeen ) {
	  crawfordDone = true;
	  crawfordSeen = false;
	}
	break;
      }
      case Action::CRAWFORD:
      {
	crawfordSeen = true;
	break;
      }
      default: break;
    }
  }


#ifdef WIN32  
    TVecAction::
#else  
    vector<Action*>::
#endif
  const_iterator start = matchRecord.end() - 1;
  for(/**/; start > matchRecord.begin(); --start) {
    if( (*start)->actionType == Action::WINS ) {
      ++start;
      break;
    }
  }

  
  currentBoard.initial();
  
  bg.setScore(matchLength - xScore, matchLength - oScore);
  
  for(/**/; start < matchRecord.end(); ++start) {
    Action const& a = **start;

    switch( a.actionType ) {
      case Action::CRAWFORD:
      {
	bg.crawford(true);
	break;
      }
      case Action::ROLLS:
      {
	break;
      }
      case Action::MOVES:
      {
	ICC(bool const ok =)
	  MoveGenerator::play(currentBoard, a.move, a.side == X ? 0 : 1);
	{                                                       assert( ok ); }
	break;
      }
      case Action::DOUBLES:
      {
	stat.cube = a.points;
	break;
      }
      case Action::TAKES:
      {
	bg.setCube(stat.cube, a.side == X ? true : false);
	break;
      }
      default:
      {
	break;
      }
    }
  }

#ifdef WIN32  
    TVecAction::
#else  
    vector<Action*>::
#endif
  const_iterator end = matchRecord.end();
  if( matchRecord.size() > 0 ) {
    --end;
    while( (*end)->side == O &&
	   (*end)->actionType == Action::REJECTS ) {
      --end;
      
      assert( (*end)->side == X &&
	      (*end)->actionType == Action::RESIGNS);
      --end;
    }
    
    if( ! ((*end)->side == X &&
	   (*end)->actionType == Action::ROLLS) ) {
      ++end;
    }
  }

  nextAction = end - matchRecord.begin();
}


Match::~Match()
{
  clearRecord();
  
  delete &bg;

  free(const_cast<char*>(stat.opponentName));
  free(const_cast<char*>(stat.computerName));
}

Match::Status const&
Match::status(void) const
{
  MatchState const& m = bg.state();

  stat.matchLength = matchLength;
  stat.xScore = xScore;
  stat.oScore = oScore;
  
  stat.cube = m.cube;
  stat.owner = m.xOwns ? X : O;
  stat.crawford = m.crawfordGame;

  stat.opTurn = opTurnOK(last());
  
  return stat;
}

Board const&
Match::board(void) const
{
  return currentBoard;
}

}
