#include "WaitJoinDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"


WaitJoinDialog::WaitJoinDialog(const GameTable& gameTable, QWidget *parent):
    MyDialog(parent),
    mJoined(false)
{
    qDebug() << "WaitJoinDialog::WaitJoinDialog";

    label = new QLabel(tr("You have created the game:") + "\n\n" +
                       tr("Bet: ") + QString::number(gameTable.bet) + "\n" +
                       tr("Move time: ") + QString::number(gameTable.time2step) + "\n" +
                       tr("Game time: ") + QString::number(gameTable.time2game) + "\n\n" +
                       tr("Please wait until an opponent joins your game..."), this);
    label->setWordWrap(true);

    exitButton = new QPushButton(tr("Exit"), this);
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));

    layout = new QVBoxLayout(this);
    layout->addWidget(label);
    layout->addWidget(exitButton);
    this->setLayout(layout);

    connect(Client::instance(), SIGNAL(opponentJoined(const Player&)), this, SLOT(onOpponentJoined(const Player&)));
    qDebug() << "connect 'opponentJoined' to WaitJoinDialog::onOpponentJoined (constructor)";
}

void WaitJoinDialog::onExitClicked()
{
    disconnect(Client::instance(), SIGNAL(opponentJoined(const Player&)), this, SLOT(onOpponentJoined(const Player&)));
    qDebug() << "disconnect 'opponentJoined' to WaitJoinDialog::onOpponentJoined (onExitClicked)";

    MainWindow::instance()->setMode(MW_WAIT);
    connect(Client::instance(), SIGNAL(gameTableDeleted()), this, SLOT(onGameTableDeleted()));
    Client::instance()->deleteGameTable(LOGIC_ID_CHESS, UI::instance()->getGameTable());
}

void  WaitJoinDialog::onGameTableDeleted()
{
    disconnect(Client::instance(), SIGNAL(opponentJoined(const Player&)), this, SLOT(onOpponentJoined(const Player&)));
    qDebug() << "disconnect 'opponentJoined' to WaitJoinDialog::onOpponentJoined (onGameTableDeleted)";

    MainWindow::instance()->showMainMenu();
}


void WaitJoinDialog::onOpponentJoined(const Player& opponent)
{
    qDebug() << "WaitJoinDialog::onOpponentJoined ";
    /*
    // Thiss should fix a bug with a double calling of onOpponentJoined
    if(mJoined)
    {
        qDebug() << "WaitJoinDialog::onOpponentJoined. Double joining refused.";
        return;
    }
    else
    {
        mJoined = true;
    }*/

    disconnect(Client::instance(), SIGNAL(opponentJoined(const Player&)), this, SLOT(onOpponentJoined(const Player&)));
    qDebug() << "disconnect 'opponentJoined' to WaitJoinDialog::onOpponentJoined ";


    int tableID = UI::instance()->getGameTable();

    UI::instance()->setPlayerName(PT_OPPONENT, opponent.name);
    UI::instance()->setPlayerRating(PT_OPPONENT, opponent.rating);

    // Get player name by ID
    QString ratingText = (opponent.rating == RATING_NOT_AVAILABLE) ? tr("not available") : QString::number(opponent.rating);
    if(MainWindow::instance()->showQuestion(opponent.name + " " + tr("wants to play chess with you") + "\n" +
              "(" + tr("opponent rating") + ": "+ ratingText + ") \n\n" +
              tr("Start the game?")))
    {
        MainWindow::instance()->setMode(MW_WAIT);
        connect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
        Client::instance()->approveGame(tableID);
    }
    else
    {
        connect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));
        Client::instance()->rejectGame(tableID);
    }
}

void WaitJoinDialog::onGameStarted()
{
    mJoined = false;
    disconnect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
    disconnect(Client::instance(), SIGNAL(opponentJoined(const Player&)), this, SLOT(onOpponentJoined(const Player&)));
    qDebug() << "disconnect 'opponentJoined' to WaitJoinDialog::onOpponentJoined (onGameStarted)";

    MainWindow::instance()->setMode(MW_WAIT);
    UI::instance()->setPlayerColor(PT_ME, PC_WHITE);
    UI::instance()->startGame();
}

void WaitJoinDialog::onGameRejected()
{
    mJoined = false;
    disconnect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));
    connect(Client::instance(), SIGNAL(opponentJoined(const Player&)), this, SLOT(onOpponentJoined(const Player&)));
    qDebug() << "connect 'opponentJoined' to WaitJoinDialog::onOpponentJoined (onGameRejected)";
}
