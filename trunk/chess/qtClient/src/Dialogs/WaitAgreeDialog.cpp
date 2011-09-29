#include "WaitAgreeDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"

WaitAgreeDialog::WaitAgreeDialog(QWidget *parent):  MyDialog(parent)
{
    label = new QLabel(tr("Please wait for start of the game..."), this);
    label->setWordWrap(true);

    exitButton = new QPushButton(tr("Exit"), this);
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));

    layout = new QVBoxLayout(this);
    layout->addWidget(label);
    layout->addWidget(exitButton);
    this->setLayout(layout);


    connect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
    connect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));
    connect(Client::instance(), SIGNAL(gameOver(int, int)), this, SLOT(onGameOver(int, int)));
}


void WaitAgreeDialog::onExitClicked()
{
    disconnect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
    disconnect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));
    disconnect(Client::instance(), SIGNAL(gameOver(int, int)), this, SLOT(onGameOver(int, int)));

    close();

    MainWindow::instance()->showMainMenu();
}



void WaitAgreeDialog::onGameStarted()
{
    disconnect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
    disconnect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));
    disconnect(Client::instance(), SIGNAL(gameOver(int, int)), this, SLOT(onGameOver(int, int)));

    MainWindow::instance()->setMode(MW_WAIT);
    UI::instance()->setPlayerColor(PT_ME, PC_BLACK);
    UI::instance()->startGame();
}

void WaitAgreeDialog::onGameRejected()
{
    disconnect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
    disconnect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));
    disconnect(Client::instance(), SIGNAL(gameOver(int, int)), this, SLOT(onGameOver(int, int)));

    MainWindow::instance()->showMessage(tr("The game host has rejected the game."));
    close();
}

void WaitAgreeDialog::onGameOver(int status, int rating)
{
    disconnect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
    disconnect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));
    disconnect(Client::instance(), SIGNAL(gameOver(int, int)), this, SLOT(onGameOver(int, int)));

    MainWindow::instance()->showMessage(tr("The game host has closed the game."));
    close();
}
