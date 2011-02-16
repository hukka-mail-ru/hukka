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
}


void WaitAgreeDialog::onExitClicked()
{
    MainWindow::instance()->showMainMenu();
}



void WaitAgreeDialog::onGameStarted()
{
    disconnect(Client::instance(), SIGNAL(gameStarted()), this, SLOT(onGameStarted()));

    MainWindow::instance()->showGameScene(PC_BLACK);
    UI::instance()->startGame();
}
