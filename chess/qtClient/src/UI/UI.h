#ifndef UI_H_
#define UI_H_

#include <QApplication>
#include <QObject>

#include <Defines.h>
#include <Pixmaps.h>

// This class is responsible for LOGIC of the game and INSTANTIATIONS of main objects
class UI: public QObject
{
    Q_OBJECT

    UI();

public:

    ~UI();

    static UI* instance()
    {
        static UI* ui = new UI();
        return ui;
    }

    void initialize(QApplication* app);
    void shutdown(); // should be called as the reaction of a disconnection from a host

    void startGame();


    void setGameTable(TABLEID id) { mGameTable = id; }
    TABLEID getGameTable() const { return mGameTable; }

    // game events
    GameState updateGameState(bool myMove, bool amIWhite);
    GameState getGameState() { return mGameState; }
    void setGameState(GameState state) { mGameState = state; }


    bool isPlayerAuthorized() { return mMe.isAuthorized; }
    void setPlayerAuthorized(bool auth) { mMe.isAuthorized = auth; }

    const Player& getPlayer(PlayerType type) { return (type == PT_ME) ? mMe : mOpponent; }

    void setPlayerName(PlayerType type, const QString& name);
    void setPlayerRating(PlayerType type, unsigned rating);
    void setPlayerColor(PlayerType type, PlayerColor color);

 //   bool isOwner() { return mIsOwner; }
 //   void setOwner(bool isOwner) { mIsOwner = isOwner; }

private:

    TABLEID mGameTable;

    GameState mGameState;
  //  bool mIsOwner; // PlayerID0 must be the owner of the game

    QApplication* mApp;

    Player mMe;
    Player mOpponent;

private slots:


    void onGameOver(const QString&, int status, int rating);
    void onDrawOffered();
    void onError(const QString& what);
    void onDisconnected();
};


#endif /* UI_H_ */
