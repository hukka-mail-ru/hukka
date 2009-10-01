// -*- C++ -*-
#if !defined( BGMATCH_H )
#define BGMATCH_H

/*
 * bgmatch.h
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
#include <iosfwd>
#include <vector>
#include "bgdefs.h"

namespace BG {

class BackGammon;

/// User Preferences and Setting
//
class Settings {
public:
  Settings(void);
  
  /// Should computer play forced moves automatically?
  //
  bool		autoForcedMoves;
  
  /// Set random number generator seed .
  //  Allows you to repeat the dice sequence.
  //
  void		setRandSeed(unsigned long seed);

  /// Play levels
  enum Level {
    ///
    BEGINNER = 0,
    ///
    INTERMEDIATE,
    ///
    EXPERT
  };

  /// Play level. 
  Level		level;
};

extern Settings settings;

  
/// Handles one Backgammon match.
//
class Match {
public:
  /// Match to @arg{matchLength}.
  //  Optionally provide opponent name in @arg{oppName}.
  //
  Match(uint matchLength, const char* opponentName = 0);

  /// Load saved match from @arg{saved}.
  //

  //  (FIXME) corrupted files result in assertion failures.

  Match(std::istream& saved);
  
  ~Match();


  /// Backgammon sides are conventionally known as @samp{X} and @samp{O}.
  //
  enum Side {X, O};

#ifndef WIN32
  /// Arbitrarily we choose computer to be O and opponent to be X.
  static Side const computerSide = O; 
#endif
  
  /// Game atomic action.
  //  Each action has a @ref{actionType,type} (what it is), a @ref{side} (who
  //  is doing it), and @ref{points,additional data} depending on type.
  //
  class Action {
  public:
    /// Action type
    enum Type {
      /// Dice is rolled.
      ROLLS,
      /// Ceckers are moved.
      MOVES,
      /// A double
      DOUBLES,
      /// A take
      TAKES,
      /// A drop
      DROPS,
      /// A resignation
      RESIGNS,
      /// A rejections
      //  Of a resignation.
      REJECTS,
      /// Game won
      WINS,
      /// Start of crawford game
      CRAWFORD
    };
    
    ///
    Action(Type t, Side s);

    /// Load action from next line in @arg{saved} file.
    Action(std::istream& saved);

    /// Save action in @arg{to}.
    //
    bool	put(std::ostream& to) const;

    /// Save action to @arg{to} in @value{SGF} format.
    bool	putSGF(std::ostream& to) const;

    ///
    Type	actionType;
    ///
    Side	side;

    /// Type specific data
    //
    union {
      ///
      //@multitable
      //@item @ref{WINS}     @tab Number of points won by side.
      //@item @ref{DOUBLES}  @tab Cube value after double.
      //@item @ref{RESIGNS}  @tab 1,2,3 for resigns normal, gammon or
      //                          backgammon.
      //@end
      //
      uint		points;

      ///
      //  Dice rolled (@ref{ROLLS}).
      int		dice[2];
      ///
      //  The move (@ref{MOVES}).
      MoveDesc		move;
    };
  };

  /// Next action to be processed by UI.
  //
  const Action*	next(void);

 /// Last action.
  //
  Action const&	last(void) const;
  
  //@
  //@node User response calls
  //  Calls made by the UI code when user makes an action
  //  Typically those return @code{false} if action is not appropriate (i.e.
  //  out of turn). Typically this indicates a bug in the calling code, and I
  //  suggest @dfn{asserting} them there, especially during development. 

  /// Called when opponent decides to roll the dice.
  bool		opRolls(void);
  
  /// Called when opponent doubles.
  bool		opDoubles(void);

  /// Check legality of partial move.
  //  Return @code{true} there is a legal move M from current board and dice
  //  where @eqn{M @subseteq @arg{move}}.
  //
  bool		partialMoveLegal(MoveDesc const& move) const;
  
  /// Opponent moves his checkers.
  //
  bool		opPlays(MoveDesc const&	move);

  /// Opponent takes the double.
  //
  bool		opTakes(void);

  /// Opponent drops (rejects the double).
  bool		opDrops(void);

  /// Opponent aceepts or rejects a resignation.
  //
  bool		opAnswers(bool accept);
  
  /// Opponent want's to resign.
  //  @arg{factor} is 1/2/3 for normal, gammon and backgammon.
  //
  void		opResigns(uint factor);

  /// Save match so far to @arg{to}.
  //
  void 		save(std::ostream& to) const;
  
  // Save match so far to @arg{to} in @value{SGF} format.
  //  Currently broken.
  //
  void		exportSGF(std::ostream& to);

  /// Save match to @arg{to} in
  /// @uref{http://jelly.effect.no,JF} @code{MAT} format.
  //
  void		exportMAT(std::ostream& to) const;
  
  /// Current match status.
  //
  struct Status {
    /// Match length.
    uint	matchLength;

    /// Opponent score.
    uint	xScore;

    /// Computer score.
    uint	oScore;

    inline bool matchOver(void) const {
      return xScore >= matchLength || oScore >= matchLength;
    }
    
    /// Current cube.
    uint	cube;

    /// Cube owner.
    //  Valid only when @arg{cube} > 1.
    //
    Side	owner;

    /// @samp{true} when game is the
    /// @uref{http://www.bkgm.com/rules/match.html,CRAWFORD} game.
    //
    bool	crawford;

    /// Is it operator turn.
    //  A slight (HACK). Needed only after loading a match, so that the UI
    //  can enable the roll/double buttons.
    //
    bool	opTurn;
    
    /// Name of computer player.
    const char*	computerName;

    /// Name of Opponent.
    const char* opponentName;
  };

  /// Get current status.
  //
  Status const&	status(void) const;

  /// Current match board.
  //  Computer is side 0, opponent is 1 regardless of whose turn it is.
  //
  //  @emph{Warning}: Board reflects the state after all @ref{Action,actions}
  //  are completed, and should not be used during looping over events.
  //
  Board	const&	board(void) const;

  static bool	applyMove(Board& b, MoveDesc const& move, uint side);

  /// Handles computer move.
  //  Find the best move using @arg{roll} and add it to match record. If game
  //  is over after move, record the win and @ref{startGame,start} a new game.
  //  If not, and opponent can't double, roll the dice for him.
  //

  bool		doMove(Action const& roll, MoveDesc const& move);
  
private:

  /// Roll dice into @arg{dice}.
  //
  void		roll(int dice[2]) const;


  /// Backgammon engine.
  //  Arbitrarily, we choose computer to be O and opponent to be X.
  //
  BackGammon&	bg;

  /// Current board.
  //  @pxref{board}.
  //
  Board		currentBoard;

  /// Match length.
  uint		matchLength;

  /// Score of X player in @ref{bg}.
  //  Opponent score.
  uint		xScore;
  
  /// Score of O player in @ref{bg}.
  //  Computer score.
  uint		oScore;

  /// Post crawford flag.
  //  When true, this game is post crawford.
  //
  bool		crawfordDone;

  /// List of actions.
  //
  std::vector<Action*>	matchRecord;
  
  /// Index of next action to report from @ref{matchRecord}.
  //  
  uint		nextAction;
 
  /// Last none reject action.
  //  Skip back over declined resignations.
  //
  Action const&		lastNoneRejectAction(void) const;
  
  
  /// Delete all actions
  void		clearRecord(void);

  /// Start a new game.
  //  Called after a game is won by either side and the appropriate score is
  //  updated. If the match is over, returns without doing anything. Otherwise
  //  starts a new game sequence.
  //
  void		startGame(void);

  /// Roll dice for computer and move.
  //  Roll the dice, add to match recored and move by calling @ref{doMove}.
  //
  void		rollAndPlay(void);

  /// @code{true} if it is opponent turn to roll/double (helper).
  //
  inline static bool	opTurnOK(Action const& prev);

  /// Storage for value returned by @ref{status}.
  //
  mutable Status	stat;
};

typedef std::vector<Match::Action*> TVecAction;

}

#endif
