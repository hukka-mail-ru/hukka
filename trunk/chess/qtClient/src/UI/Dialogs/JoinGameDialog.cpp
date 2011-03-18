#include <QTableWidgetItem>
#include "JoinGameDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"
#include <XML.h>

const int COLUMN_FOR_NAME        = 0;
const int COLUMN_FOR_RATING      = 1;
const int COLUMN_FOR_TIME2STEP  = 2;
const int COLUMN_FOR_TIME2GAME  = 3;

const int NUMBER_OF_COLUMNS = 4;

JoinGameDialog::JoinGameDialog(const QList<GameTable>& tables, QWidget *parent):
    MyDialog(parent),
    mGameTables(tables),
    mCounter(0)
{
    setWindowTitle(tr("Join Game"));

    okButton = new QPushButton(tr("OK"), this);
    cancelButton = new QPushButton(tr("Cancel"), this);

    connect(okButton, SIGNAL(clicked()), this, SLOT(onOkClicked()));
    connect(cancelButton, SIGNAL(clicked()), this, SLOT(onCancelClicked()));

    QStringList labels;
    labels << tr("Name") << tr("Rating") << tr("Move time") << tr("Game time");
    tableWidget = new QTableWidget(0, NUMBER_OF_COLUMNS, this);
    tableWidget->setHorizontalHeaderLabels(labels);

    upperLayout = new QHBoxLayout();
    upperLayout->addWidget(tableWidget);

    lowerLayout = new QHBoxLayout();
    lowerLayout->addWidget(okButton);
    lowerLayout->addWidget(cancelButton);

    layout = new QVBoxLayout(this);
    layout->addLayout(upperLayout);
    layout->addLayout(lowerLayout);


    setLayout(layout);

    int margin = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_SCENE << XML_NODE_MARGIN << XML_NODE_WIDTH).toInt();

    tableWidget->setColumnWidth(COLUMN_FOR_NAME,      MainWindow::instance()->width()/NUMBER_OF_COLUMNS - 2 * margin);
    tableWidget->setColumnWidth(COLUMN_FOR_RATING,    MainWindow::instance()->width()/NUMBER_OF_COLUMNS - 2 * margin);
    tableWidget->setColumnWidth(COLUMN_FOR_TIME2STEP, MainWindow::instance()->width()/NUMBER_OF_COLUMNS - 2 * margin);
    tableWidget->setColumnWidth(COLUMN_FOR_TIME2GAME, MainWindow::instance()->width()/NUMBER_OF_COLUMNS - 2 * margin);

    if(!mGameTables.empty())
        getParams(&mGameTables[0]);
}


void JoinGameDialog::getParams(GameTable* table)
{
    mTableToGetParams = table;

   // qDebug() << "JoinGameDialog::getPlayerName. tableID = " << tableID;
    connect(Client::instance(),
            SIGNAL(gotGameTableParams(const GameTable&)),
            this,
            SLOT(onGotGameTableParams(const GameTable&)));

    Client::instance()->getGameTableParams(LOGIC_ID_CHESS, table->id);
}

void JoinGameDialog::onGotGameTableParams(const GameTable& table)
{
   //qDebug() << "JoinGameDialog::onGotGameTableParams. name = " << name;

    disconnect(Client::instance(),
            SIGNAL(gotGameTableParams(const GameTable&)),
            this,
            SLOT(onGotGameTableParams(const GameTable&)));

    //save the result
    mTableToGetParams->operator = (table);

    // fill one row of the table
    tableWidget->insertRow(mCounter);

    QTableWidgetItem* nameItem = new QTableWidgetItem(table.host.name);
    tableWidget->setItem(mCounter, COLUMN_FOR_NAME, nameItem);

    QString ratingText = (table.host.rating == RATING_NOT_AVAILABLE) ? tr("N/A") : QString::number(table.host.rating);
    QTableWidgetItem* ratingItem = new QTableWidgetItem(ratingText);
    tableWidget->setItem(mCounter, COLUMN_FOR_RATING, ratingItem);

    QTableWidgetItem* time2stepItem = new QTableWidgetItem(Global::seconds2hrs(table.time2step));
    tableWidget->setItem(mCounter, COLUMN_FOR_TIME2STEP, time2stepItem);

    QTableWidgetItem* time2gameItem = new QTableWidgetItem(Global::seconds2hrs(table.time2game));
    tableWidget->setItem(mCounter, COLUMN_FOR_TIME2GAME, time2gameItem);

    mCounter++;

   // qDebug() << "mCounter = " << mCounter << " mGameTables.size() = " << mGameTables.size();

    // recursively call again (until all the rows of the table are filled)
    if(mCounter < mGameTables.size())
        getParams(&mGameTables[mCounter]);
}



void JoinGameDialog::onOkClicked()
{
   // qDebug() << "table.currentRow ()" << tableWidget->currentRow();
  //  qDebug() << " mGameTables[tableWidget->currentRow()]" <<  mGameTables[tableWidget->currentRow()];

    if(tableWidget->currentRow() < 0)
        return;

    GameTable& currentTable = mGameTables[tableWidget->currentRow()];

    MainWindow::instance()->setMode(MW_WAIT);

    // save name and rating of selected opponent
    UI::instance()->setPlayerName(PT_OPPONENT, currentTable.host.name);
    UI::instance()->setPlayerRating(PT_OPPONENT, currentTable.host.rating);

    connect(Client::instance(), SIGNAL(joined(TABLEID)), this, SLOT(onJoined(TABLEID)));
    Client::instance()->joinGame( currentTable.id );
}

void JoinGameDialog::onJoined(TABLEID id)
{
    disconnect(Client::instance(), SIGNAL(joined(TABLEID)), this, SLOT(onJoined(TABLEID)));

    UI::instance()->setGameTable(id);
    MainWindow::instance()->showWaitAgreeDialog();
}


void JoinGameDialog::onCancelClicked()
{
    MainWindow::instance()->showFindGameDialog();
}
