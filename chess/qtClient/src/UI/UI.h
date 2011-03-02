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
    void surrender();
 //   void stopGame();

    void setGameTable(TABLEID id) { mGameTable = id; }
    TABLEID getGameTable() const { return mGameTable; }

    // game events
    void cellClicked(CELLID cell);
    GameState updateField(const Field& field, bool myMove, bool amIWhite);
    GameState getGameState() { return mGameState; }


    bool isPlayerAuthorized() { return mPlayerAuthotized; }
    void setPlayerAuthorized(bool auth) { mPlayerAuthotized = auth; }

private:

    bool isEnemyPiece(CELLID cell);

    Field mField;

    TABLEID mGameTable;

    PlayerColor mPlayerColor;
    GameState mGameState;
    Move mMove;
    bool mPlayerAuthotized;

    QApplication* mApp;

private slots:

    void onInvalidMove();
    void onGameOver(const QString&);
    void onDrawOffered();
    void onError(const QString& what);
    void onDisconnected();
};


#endif /* UI_H_ */
