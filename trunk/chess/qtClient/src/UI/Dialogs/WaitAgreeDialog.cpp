#include "WaitAgreeDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"

WaitAgreeDialog::WaitAgreeDialog(QWidget *parent):  MyDialog(parent)
{
    label = new QLabel(tr("Wait for start..."), this);
    exitButton = new QPushButton(tr("Exit"), this);
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));

    layout = new QVBoxLayout(this);
    layout->addWidget(label);
    layout->addWidget(exitButton);
    this->setLayout(layout);


    connect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
    connect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));
}


void WaitAgreeDialog::onExitClicked()
{
    MainWindow::instance()->showMainMenu();
}



void WaitAgreeDialog::onGameStarted()
{
    disconnect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
    disconnect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));

    MainWindow::instance()->showGameScene(PC_BLACK);
    UI::instance()->startGame();
}

void WaitAgreeDialog::onGameRejected()
{
    disconnect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));
    disconnect(Client::instance(), SIGNAL(gameRejected()), this, SLOT(onGameRejected()));

    MainWindow::instance()->showMessage("The game host has rejected the game.");
    close();
}
