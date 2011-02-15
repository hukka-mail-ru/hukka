#include "GameDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"

GameDialog::GameDialog(QWidget *parent): Dialog(parent)
{
    setWindowTitle(tr("Game"));

    surrenderButton = new QPushButton(tr("Surrender"), this);
    drawButton = new QPushButton(tr("Draw"), this);
    returnButton = new QPushButton(tr("Return to game"), this);

    connect(surrenderButton, SIGNAL(clicked()), this, SLOT(onSurrenderClicked()));
    connect(drawButton, SIGNAL(clicked()), this, SLOT(onDrawClicked()));
    connect(returnButton, SIGNAL(clicked()), this, SLOT(onReturnClicked()));

    layout = new QVBoxLayout(this);
    layout->addWidget(surrenderButton);
    layout->addWidget(drawButton);
    layout->addWidget(returnButton);
    layout->addStretch();

    setLayout(layout);
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
        qDebug() << "GameDialog::onDrawClicked";
        MainWindow::instance()->showWaitDrawDialog();
    }
}



void GameDialog::onReturnClicked()
{
    close();
}
