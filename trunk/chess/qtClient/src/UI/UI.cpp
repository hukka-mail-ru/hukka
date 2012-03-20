#include <assert.h>
#include <QDebug>
#include "UI.h"
#include "Exception.h"
#include "Client.h"
#include "MainWindow.h"
#include "Pixmaps.h"


void UI::initialize(QApplication* app)
{
    mGameTable = 0;
    mMe.isAuthorized = false;

  //  qDebug() << "UI::initialize()";

    connect(Client::instance(), SIGNAL(error(const QString&)), this, SLOT(onError(const QString&)));

    MainWindow::instance()->initialize();

    mApp = app;

    mGameOverTimer = new QTimer(this);
 //   qDebug() << "UI::initialize() ok";
}


void UI::shutdown()
{
    // Client must properly send all possible messages in its queue.
    connect(Client::instance(), SIGNAL(disconnectedFromHost()), this, SLOT(onDisconnected()));
    Client::instance()->disconnectFromHost();
}

void UI::onDisconnected()
{
    disconnect(Client::instance(), SIGNAL(disconnectedFromHost()), this, SLOT(onDisconnected()));

    mApp->exit(0);
}


void UI::startGame()
{
    qDebug() << "UI::startGame()";

    MainWindow::instance()->closeCurrentDialog();

    if(mMe.color == PC_WHITE)
    {
        mGameState = GS_WAIT_FOR_PLAYER_TOUCH;
    }
    else
    {
        mGameState = GS_WAIT_FOR_SERVER;
    }

    connect(Client::instance(), SIGNAL(gameOver(int, int)), this, SLOT(onGameOver(int, int)));
    connect(Client::instance(), SIGNAL(drawOffered()), this, SLOT(onDrawOffered()));

    assert(mGameTable);

    Client::instance()->getPosition(mGameTable);
}



void UI::onGameOver(int status, int rating)
{
    disconnect(Client::instance(), SIGNAL(gameOver(int, int)), this, SLOT(onGameOver(int, int)));
    disconnect(Client::instance(), SIGNAL(drawOffered()), this, SLOT(onDrawOffered()));

    // getPosition must be earlier than showMessage because the clocks must not tick while
    // the user is looking at the message box.
    Client::instance()->getPosition(mGameTable); // player must see the victory move
    Client::instance()->deleteLastGameResult();

    mGameOverText = Global::getGameResultText(status, rating) + "\n";

    mGameState = GS_GAME_OVER;
    MainWindow::instance()->updateGameScene();

    // a timeout before showing the GameDialog -
    // so the user can enjoy the victory:)
    mGameOverTimer->start(GAME_OVER_TIMEOUT * 1000);
    connect(mGameOverTimer, SIGNAL(timeout()), this, SLOT(onGameOverTimeout()));
}

void UI::onGameOverTimeout()
{
    mGameOverTimer->stop();
    disconnect(mGameOverTimer, SIGNAL(timeout()), this, SLOT(onGameOverTimeout()));

    MainWindow::instance()->showGameDialog(mGameOverText);
}



void UI::onDrawOffered()
{
    disconnect(Client::instance(), SIGNAL(drawOffered()), this, SLOT(onDrawOffered()));

    bool accept = MainWindow::instance()->showQuestion(tr("Your opponent has offered a draw. Agree?"));

    Client::instance()->replyDraw(UI::instance()->getGameTable(), accept);

    connect(Client::instance(), SIGNAL(drawOffered()), this, SLOT(onDrawOffered()));
}

void UI::onError(const QString& what)
{
    disconnect(Client::instance(), SIGNAL(error(const QString&)), this, SLOT(onError(const QString&)));

 //   qDebug() << "UI::onError";
    MainWindow::instance()->closeCurrentDialog();
    MainWindow::instance()->showError(what);

    connect(Client::instance(), SIGNAL(error(const QString&)), this, SLOT(onError(const QString&)));
}




GameState UI::updateGameState(bool myMove, bool iAmWhite)
{
    // pass the move
    mMe.color = iAmWhite ? PC_WHITE : PC_BLACK;
    mOpponent.color = iAmWhite ? PC_BLACK : PC_WHITE;

    mGameState = myMove ? GS_WAIT_FOR_PLAYER_TOUCH : GS_WAIT_FOR_OPPONENT;


    return mGameState;
}


void UI::setPlayerName(PlayerType type, const QString& name)
{
    switch(type)
    {
        case PT_ME:       mMe.name = name; break;
        case PT_OPPONENT: mOpponent.name = name; break;
        default: break;
    }
}

void UI::setPlayerRating(PlayerType type, unsigned rating)
{
    switch(type)
    {
        case PT_ME:       mMe.rating = rating; break;
        case PT_OPPONENT: mOpponent.rating = rating; break;
        default: break;
    }
}

void UI::setPlayerColor(PlayerType type, PlayerColor color)
{
    switch(type)
    {
        case PT_ME:       mMe.color = color; break;
        case PT_OPPONENT: mOpponent.color = color; break;
        default: break;
    }
}

void UI::setPlayerBalance(PlayerType type, unsigned balance)
{
    switch(type)
    {
        case PT_ME:       mMe.balance = balance; break;
        case PT_OPPONENT: mOpponent.balance = balance; break;
        default: break;
    }
}






