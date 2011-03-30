#include "GameDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"

GameDialog::GameDialog(QWidget *parent): MyDialog(parent)
{
    setWindowTitle(tr("Game"));
    layout = new QVBoxLayout(this);



    if(UI::instance()->getGameState() == GS_GAME_OVER)
    {
        returnToMenuButton = new QPushButton(tr("Return to main menu"), this);
        connect(returnToMenuButton, SIGNAL(clicked()), this, SLOT(onReturnToMenuClicked()));
        layout->addWidget(returnToMenuButton);
    }
    else
    {
        surrenderButton = new QPushButton(tr("Surrender"), this);
        connect(surrenderButton, SIGNAL(clicked()), this, SLOT(onSurrenderClicked()));
        layout->addWidget(surrenderButton);

        drawButton = new QPushButton(tr("Draw"), this);
        connect(drawButton, SIGNAL(clicked()), this, SLOT(onDrawClicked()));
        layout->addWidget(drawButton);
    }

    returnButton = new QPushButton(tr("Return to game"), this);
    connect(returnButton, SIGNAL(clicked()), this, SLOT(onReturnClicked()));
    layout->addWidget(returnButton);

    layout->addStretch();
    setLayout(layout);
}


void GameDialog::onReturnToMenuClicked()
{
    MainWindow::instance()->showMainMenu();
}


void GameDialog::onSurrenderClicked()
{
    if((MainWindow::instance()->showQuestion(tr("Do you want to surrender?"))))
    {
        MainWindow::instance()->setMode(MW_WAIT);

        TABLEID id = UI::instance()->getGameTable();
        Client::instance()->surrender(id);
    }
}

void GameDialog::onDrawClicked()
{
    if((MainWindow::instance()->showQuestion(tr("Do you want to offer a draw?"))))
    {
        TABLEID id = UI::instance()->getGameTable();
        Client::instance()->offerDraw(id);
        //qDebug() << "GameDialog::onDrawClicked";
        MainWindow::instance()->showWaitDrawDialog();
    }
}



void GameDialog::onReturnClicked()
{
    close();
}
