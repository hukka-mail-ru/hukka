#include "FindGameDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"

FindGameDialog::FindGameDialog(QWidget *parent): MyDialog(parent)
{
  /*  setWindowTitle(tr("Find Game"));

    okButton = new QPushButton(tr("All Games"), this);
    randomGameButton = new QPushButton(tr("Random Game"), this);
    cancelButton = new QPushButton(tr("Cancel"), this);

    //connect(okButton, SIGNAL(clicked()), this, SLOT(onOkClicked()));
   // connect(randomGameButton, SIGNAL(clicked()), this, SLOT(onRandomGameClicked()));
   // connect(cancelButton, SIGNAL(clicked()), this, SLOT(onCancelClicked()));


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
*/
    MainWindow::instance()->setMode(MW_WAIT);

    // find all the games where PARAMETER_ID_TIME2GAME > 0
    Param gameTime = { PARAMETER_ID_GAMETIME, 0, OPERATOR_MORE, OPERATOR_LAST };

    QList<Param> params;
    params << gameTime;

    connect(Client::instance(), SIGNAL(gotGameTables(const QList<GameTable>&)), this, SLOT(onGotGameTables(const QList<GameTable>&)));
    Client::instance()->findGameTables(LOGIC_ID_CHESS, DEFAULT_MAXCOUNT, params);
}



/*
void FindGameDialog::onCancelClicked()
{
    qDebug() << "FindGameDialog::onCancelClicked()";
    disconnect(Client::instance(), SIGNAL(gotGameTables(const QList<GameTable>&)), this, SLOT(onGotGameTables(const QList<GameTable>&)));

    MainWindow::instance()->showMainMenu();
}


void FindGameDialog::onRandomGameClicked()
{
    MainWindow::instance()->setMode(MW_WAIT);

    QList<Param> empty;

    connect(Client::instance(), SIGNAL(gotGameTables(const QList<GameTable>&)), this, SLOT(onGotGameTables(const QList<GameTable>&)));
    Client::instance()->getRandomGameTable(LOGIC_ID_CHESS, empty);
}
*/

void FindGameDialog::onGotGameTables(const QList<GameTable>& tables)
{
    disconnect(Client::instance(), SIGNAL(gotGameTables(const QList<GameTable>&)), this, SLOT(onGotGameTables(const QList<GameTable>&)));

    for(int i=0; i<tables.size(); i++)
        mTables << tables[i];

    if(tables.empty() || (tables.size() == 1 && tables[0].id == 0))
    {
        MainWindow::instance()->showError(tr("No games now. Try again later or create a new game."));
        hide();
    }
    else
    {
       // not to show the empty JoinGameDialog, fetch parameters of the first found table.
        connect(Client::instance(),
                SIGNAL(gotGameTableParams(const GameTable&)),
                this,
                SLOT(onGotGameTableParams(const GameTable&)));
        Client::instance()->getGameTableParams(LOGIC_ID_CHESS, tables[0].id);

    }
}


void FindGameDialog::onGotGameTableParams(const GameTable& table)
{
    disconnect(Client::instance(),
            SIGNAL(gotGameTableParams(const GameTable&)),
            this,
            SLOT(onGotGameTableParams(const GameTable&)));

    mTables[0] = table;

    MainWindow::instance()->showJoinGameDialog(mTables);
}



