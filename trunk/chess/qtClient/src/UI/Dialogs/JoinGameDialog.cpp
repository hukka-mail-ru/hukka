#include <QTableWidgetItem>
#include "JoinGameDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"
#include <XML.h>

#define COLUMN_FOR_NAME         0
#define COLUMN_FOR_RATING       1
#define COLUMN_FOR_TIME2STEP    2
#define COLUMN_FOR_TIME2GAME    3

#define NUMBER_OF_COLUMNS 4

JoinGameDialog::JoinGameDialog(const QList<TABLEID>& tableIDs, QWidget *parent):
    MyDialog(parent),
    mGameTableIDs(tableIDs),
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

    if(!mGameTableIDs.empty())
        getParams(mGameTableIDs[0]);
}


void JoinGameDialog::getParams(TABLEID tableID)
{
   // qDebug() << "JoinGameDialog::getPlayerName. tableID = " << tableID;
    connect(Client::instance(),
            SIGNAL(gotGameTableParams(const QString&, qint32, qint32, qint32)),
            this,
            SLOT(onGotGameTableParams(const QString&, qint32, qint32, qint32)));

    Client::instance()->getGameTableParams(LOGIC_ID_CHESS, tableID);
}

void JoinGameDialog::onGotGameTableParams(const QString& name, qint32 rating,
                                          qint32 time2step, qint32 time2game)
{
   //qDebug() << "JoinGameDialog::onGotGameTableParams. name = " << name;

    disconnect(Client::instance(),
            SIGNAL(gotGameTableParams(const QString&, qint32, qint32, qint32)),
            this,
            SLOT(onGotGameTableParams(const QString&, qint32, qint32, qint32)));

    // fill one row of the table
    tableWidget->insertRow(mCounter);

    QTableWidgetItem* nameItem = new QTableWidgetItem(name);
    tableWidget->setItem(mCounter, COLUMN_FOR_NAME, nameItem);

    QString ratingText = (rating == RATING_NOT_AVAILABLE) ? "N/A" : QString::number(rating);
    QTableWidgetItem* ratingItem = new QTableWidgetItem(ratingText);
    tableWidget->setItem(mCounter, COLUMN_FOR_RATING, ratingItem);

    QTableWidgetItem* time2stepItem = new QTableWidgetItem(Game::seconds2hrs(time2step));
    tableWidget->setItem(mCounter, COLUMN_FOR_TIME2STEP, time2stepItem);

    QTableWidgetItem* time2gameItem = new QTableWidgetItem(Game::seconds2hrs(time2game));
    tableWidget->setItem(mCounter, COLUMN_FOR_TIME2GAME, time2gameItem);

    mCounter++;

   // qDebug() << "mCounter = " << mCounter << " mGameTableIDs.size() = " << mGameTableIDs.size();

    // recursively call again (until all the rows of the table are filled)
    if(mCounter < mGameTableIDs.size())
        getParams(mGameTableIDs[mCounter]);
}



void JoinGameDialog::onOkClicked()
{
   // qDebug() << "table.currentRow ()" << tableWidget->currentRow();
  //  qDebug() << " mGameTableIDs[tableWidget->currentRow()]" <<  mGameTableIDs[tableWidget->currentRow()];

    if(tableWidget->currentRow() < 0)
        return;


    MainWindow::instance()->setMode(MW_WAIT);

    connect(Client::instance(), SIGNAL(joined(TABLEID)), this, SLOT(onJoined(TABLEID)));
    Client::instance()->joinGame( mGameTableIDs[tableWidget->currentRow()] );
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
