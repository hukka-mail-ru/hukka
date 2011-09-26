#include <QTableWidgetItem>
#include <assert.h>
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
    mGameTables(tables)
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

    connect(tableWidget, SIGNAL(cellPressed(int, int)), this, SLOT(onCellPressed(int, int)));
    connect(tableWidget, SIGNAL(cellClicked(int, int)), this, SLOT(onCellClicked(int, int)));

    mCurrentTable = 0;
    if(!mGameTables.empty())
        onGotGameTableParams(mGameTables[mCurrentTable]);
}


void JoinGameDialog::onCellPressed(int row, int column)
{
    Q_UNUSED(column);
    tableWidget->setRangeSelected(QTableWidgetSelectionRange(row, 0 ,row, tableWidget->columnCount()-1), true);
}
void JoinGameDialog::onCellClicked(int row, int column)
{
    Q_UNUSED(column);
    tableWidget->setRangeSelected(QTableWidgetSelectionRange(row, 0 ,row, tableWidget->columnCount()-1), true);
}



void JoinGameDialog::getParams(const GameTable& table)
{
    qDebug() << "JoinGameDialog::getPlayerName. mCurrentTable = " << mCurrentTable;
    connect(Client::instance(),
            SIGNAL(gotGameTableParams(const GameTable&)),
            this,
            SLOT(onGotGameTableParams(const GameTable&)));

    Client::instance()->getGameTableParams(LOGIC_ID_CHESS, table.id);
}

void JoinGameDialog::onGotGameTableParams(const GameTable& table)
{
   //qDebug() << "JoinGameDialog::onGotGameTableParams. name = " << name;

    disconnect(Client::instance(),
            SIGNAL(gotGameTableParams(const GameTable&)),
            this,
            SLOT(onGotGameTableParams(const GameTable&)));

    //save the result
    mGameTables[mCurrentTable] = table;

    // fill one row of the table
    tableWidget->insertRow(mCurrentTable);

    QTableWidgetItem* nameItem = new QTableWidgetItem(table.host.name);
    tableWidget->setItem(mCurrentTable, COLUMN_FOR_NAME, nameItem);

    QString ratingText = (table.host.rating == RATING_NOT_AVAILABLE) ? tr("N/A") : QString::number(table.host.rating);
    QTableWidgetItem* ratingItem = new QTableWidgetItem(ratingText);
    tableWidget->setItem(mCurrentTable, COLUMN_FOR_RATING, ratingItem);

    QTableWidgetItem* time2stepItem = new QTableWidgetItem(Global::seconds2hrs(table.time2step));
    tableWidget->setItem(mCurrentTable, COLUMN_FOR_TIME2STEP, time2stepItem);

    QTableWidgetItem* time2gameItem = new QTableWidgetItem(Global::seconds2hrs(table.time2game));
    tableWidget->setItem(mCurrentTable, COLUMN_FOR_TIME2GAME, time2gameItem);

    mCurrentTable++;

    qDebug() << "mCurrentTable = " << mCurrentTable << " mGameTables.size() = " << mGameTables.size();

    // recursively call again (until all the rows of the table are filled)
    if(mCurrentTable < mGameTables.size())
        getParams(mGameTables[mCurrentTable]);
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
    MainWindow::instance()->showMainMenu();
}
