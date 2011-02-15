#include "FindGameDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"

FindGameDialog::FindGameDialog(QWidget *parent): Dialog(parent)
{
    setWindowTitle(tr("Find Game"));

    okButton = new QPushButton(tr("All Games"), this);
    randomGameButton = new QPushButton(tr("Random Game"), this);
    cancelButton = new QPushButton(tr("Cancel"), this);

    connect(okButton, SIGNAL(clicked()), this, SLOT(onOkClicked()));
    connect(randomGameButton, SIGNAL(clicked()), this, SLOT(onRandomGameClicked()));
    connect(cancelButton, SIGNAL(clicked()), this, SLOT(onCancelClicked()));


    edits = new QGridLayout();

    upperLayout = new QVBoxLayout();
    upperLayout->addLayout(edits);
    upperLayout->addStretch();

    lowerLayout = new QHBoxLayout();
    lowerLayout->addWidget(okButton);
    lowerLayout->addWidget(randomGameButton);
    lowerLayout->addWidget(cancelButton);

    layout = new QVBoxLayout(this);
    layout->addLayout(upperLayout);
    layout->addLayout(lowerLayout);

    setLayout(layout);
}

void FindGameDialog::onOkClicked()
{
    MainWindow::instance()->setMode(MW_WAIT);

    // find all the games where PARAMETER_ID_TIME2GAME > 0
    Param gameTime = { PARAMETER_ID_GAMETIME, 0, OPERATOR_MORE, OPERATOR_LAST };

    QList<Param> params;
    params << gameTime;

    connect(Client::instance(), SIGNAL(gotGameTables(const QList<TABLEID>&)), this, SLOT(onGotGameTables(const QList<TABLEID>&)));
    Client::instance()->findGameTables(LOGIC_ID_CHESS, DEFAULT_MAXCOUNT, params);
}




void FindGameDialog::onCancelClicked()
{
    MainWindow::instance()->showMainMenu();
}


void FindGameDialog::onRandomGameClicked()
{
    MainWindow::instance()->setMode(MW_WAIT);

    QList<Param> empty;

    connect(Client::instance(), SIGNAL(gotGameTables(const QList<TABLEID>&)), this, SLOT(onGotGameTables(const QList<TABLEID>&)));
    Client::instance()->getRandomGameTable(LOGIC_ID_CHESS, empty);
}


void FindGameDialog::onGotGameTables(const QList<TABLEID>& ids)
{
    disconnect(Client::instance(), SIGNAL(gotGameTables(const QList<TABLEID>&)), this, SLOT(onGotGameTables(const QList<TABLEID>&)));

    if(ids.empty() || (ids.size() == 1 && ids[0] == 0))
    {
        MainWindow::instance()->showError(tr("No game table found"));
        MainWindow::instance()->showFindGameDialog();
    }
    else
    {
        qDebug() << "onGotGameTables(const QList<TABLEID>& ids) " << ids.size();
        MainWindow::instance()->showJoinGameDialog(ids);
    }
}





