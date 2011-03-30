#include "WaitJoinDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"


WaitJoinDialog::WaitJoinDialog(QWidget *parent):   MyDialog(parent)
{
    qDebug() << "WaitJoinDialog::WaitJoinDialog";

    label = new QLabel(tr("Please wait until an opponent joins your game..."), this);
    exitButton = new QPushButton(tr("Exit"), this);
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));

    layout = new QVBoxLayout(this);
    layout->addWidget(label);
    layout->addWidget(exitButton);
    this->setLayout(layout);

    connect(Client::instance(), SIGNAL(opponentJoined(const QString&, int)), this, SLOT(onOpponentJoined(const QString&, int)));
}

void WaitJoinDialog::onExitClicked()
{
    MainWindow::instance()->setMode(MW_WAIT);
    connect(Client::instance(), SIGNAL(gameTableDeleted()), this, SLOT(onGameTableDeleted()));
    Client::instance()->deleteGameTable(LOGIC_ID_CHESS, UI::instance()->getGameTable());
}

void  WaitJoinDialog::onGameTableDeleted()
{
    MainWindow::instance()->showMainMenu();
}


void WaitJoinDialog::onOpponentJoined(const QString& opponentName, int opponentRating)
{
    static bool joined = false;

    // Thiss should fix a bug with a double calling of onOpponentJoined
    if(joined)
    {
        qDebug() << "WaitJoinDialog::onOpponentJoined. Double joining refused.";
        return;
    }
    else
    {
        joined = true;
    }

    disconnect(Client::instance(), SIGNAL(opponentJoined(const QString&, int)), this, SLOT(onOpponentJoined(const QString&, int)));
 //   qDebug() << "WaitJoinDialog::onOpponentJoined " << res;

    int tableID = UI::instance()->getGameTable();

    UI::instance()->setPlayerName(PT_OPPONENT, opponentName);
    UI::instance()->setPlayerRating(PT_OPPONENT, opponentRating);

    // Get player name by ID
    QString ratingText = (opponentRating == RATING_NOT_AVAILABLE) ? tr("is not available") : QString::number(opponentRating);
    if(MainWindow::instance()->showQuestion(opponentName + tr(" (rating ") + ratingText + tr(") wants to play chess with you. Agree?")))
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
    disconnect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));

   // MainWindow::instance()->showGameScene(PC_WHITE);
    MainWindow::instance()->setMode(MW_WAIT);
    UI::instance()->setPlayerColor(PT_ME, PC_WHITE);
    UI::instance()->startGame();
}

void WaitJoinDialog::onGameRejected()
{
    disconnect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));
    connect(Client::instance(), SIGNAL(opponentJoined(const QString&)), this, SLOT(onOpponentJoined(const QString&)));
}
