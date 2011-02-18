#include "WaitJoinDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"


WaitJoinDialog::WaitJoinDialog(QWidget *parent):   MyDialog(parent)
{
    label = new QLabel(tr("Wait for oppponent..."), this);
    exitButton = new QPushButton(tr("Exit"), this);
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));

    layout = new QVBoxLayout(this);
    layout->addWidget(label);
    layout->addWidget(exitButton);
    this->setLayout(layout);

    connect(Client::instance(), SIGNAL(opponentJoined(const QString&)), this, SLOT(onOpponentJoined(const QString&)));
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


void WaitJoinDialog::onOpponentJoined(const QString& opponentName)
{
    disconnect(Client::instance(), SIGNAL(opponentJoined(const QString&)), this, SLOT(onOpponentJoined(const QString&)));
    qDebug() << "WaitJoinDialog::onOpponentJoined";

    int tableID = UI::instance()->getGameTable();

    // TODO Get player name by ID
    if(MainWindow::instance()->showQuestion(opponentName + " wants to play chess with you. Agree?"))
    {
        MainWindow::instance()->setMode(MW_WAIT);
        connect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
        Client::instance()->startGame(tableID);
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

    MainWindow::instance()->showGameScene(PC_WHITE);
    UI::instance()->startGame();
}

void WaitJoinDialog::onGameRejected()
{
    disconnect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));
    connect(Client::instance(), SIGNAL(opponentJoined(const QString&)), this, SLOT(onOpponentJoined(const QString&)));
}
