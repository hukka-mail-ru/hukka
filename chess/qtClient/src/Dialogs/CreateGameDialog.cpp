#include "CreateGameDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"


CreateGameDialog::CreateGameDialog(QWidget *parent): MyDialog(parent)
{
    setWindowTitle(tr("Create Game"));

    unsigned balance = UI::instance()->getPlayer(PT_ME).balance;
    betLabel = new QLabel(tr("Bet (no more than ") + QString::number(balance) + tr(" RUR)"), this);
    moveTimeLabel = new QLabel(tr("Move time (min)"), this);
    gameTimeLabel = new QLabel(tr("Game time (min)"), this);
    minRatingLabel = new QLabel(tr("Min opponent rating"), this);
    maxRatingLabel = new QLabel(tr("Max opponent rating"), this);

    betEdit = new QLineEdit(QString::number(DEFAULT_BET), this);
    moveTimeEdit = new QLineEdit(QString::number(DEFAULT_MOVETIME), this);
    gameTimeEdit = new QLineEdit(QString::number(DEFAULT_GAMETIME), this);
    minRatingEdit  = new QLineEdit(QString::number(DEFAULT_MINRATING), this);
    maxRatingEdit = new QLineEdit(QString::number(DEFAULT_MAXRATING), this);

    okButton = new QPushButton(tr("OK"), this);
    exitButton = new QPushButton(tr("Exit"), this);


    edits = new QGridLayout();

    edits->addWidget(betLabel,       0, 0);
    edits->addWidget(moveTimeLabel,  2, 0);
    edits->addWidget(gameTimeLabel,  4, 0);
    edits->addWidget(minRatingLabel, 6, 0);
    edits->addWidget(maxRatingLabel, 8, 0);

    edits->addWidget(betEdit,        0, 2);
    edits->addWidget(moveTimeEdit,   2, 2);
    edits->addWidget(gameTimeEdit,   4, 2);
    edits->addWidget(minRatingEdit,  6, 2);
    edits->addWidget(maxRatingEdit,  8, 2);



    upperLayout = new QVBoxLayout();
    upperLayout->addLayout(edits);
    upperLayout->addStretch();

    lowerLayout = new QHBoxLayout();
    lowerLayout->addWidget(okButton);
    lowerLayout->addWidget(exitButton);

    layout = new QVBoxLayout(this);
    layout->addLayout(upperLayout);
    layout->addLayout(lowerLayout);

    connect(okButton, SIGNAL(clicked()), this, SLOT(onOkClicked()));
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));

    setLayout(layout);
}

bool checkInt(const QString& parameter, unsigned& value, const QString& description)
{
    bool ok;
    if(parameter.toInt(&ok) < 0 || !ok)
    {
        MainWindow::instance()->showMessage(QObject::tr("Invalid value: ") + description);
        return false;
    }

    value = parameter.toInt();
    return true;
}


void CreateGameDialog::onOkClicked()
{

    Param bet;
    bet.id = PARAMETER_ID_BET;
    if(!checkInt(betEdit->text(), bet.value, tr("bet")))
        return;

    unsigned balance = UI::instance()->getPlayer(PT_ME).balance;
    if(bet.value > balance)
    {
        MainWindow::instance()->showMessage(tr("Bet is too high! Maximum is ") +
                QString::number(balance) + tr(" RUR"));
        return;
    }

    Param moveTime;
    moveTime.id = PARAMETER_ID_MOVETIME;
    if(!checkInt(moveTimeEdit->text(), moveTime.value, tr("move time")))
        return;
    if(moveTime.value < MINIMAL_TIME)
    {
        MainWindow::instance()->showMessage(tr("Move time is too low! Minimum is ") +
                QString::number(MINIMAL_TIME) + tr(" min."));
        return;
    }
    moveTime.value *= SECONDS_IN_MINUTE;


    Param gameTime;
    gameTime.id = PARAMETER_ID_GAMETIME;
    if(!checkInt(gameTimeEdit->text(), gameTime.value, tr("game time")))
        return;
    if(gameTime.value < MINIMAL_TIME)
    {
        MainWindow::instance()->showMessage(tr("Game time is too low! Minimum is ") +
                QString::number(MINIMAL_TIME) + tr(" min."));
        return;
    }
    gameTime.value *= SECONDS_IN_MINUTE;


    Param maxRating;
    maxRating.id = PARAMETER_ID_MAXRATING;
    if(!checkInt(maxRatingEdit->text(), maxRating.value, tr("max. rating")))
        return;

    Param minRating;
    minRating.id = PARAMETER_ID_MINRATING;
    if(!checkInt(minRatingEdit->text(), minRating.value, tr("min. rating")))
        return;

    mGameTable.bet = bet.value;
    mGameTable.time2step = moveTime.value;
    mGameTable.time2game = gameTime.value;

    QList<Param> params;
    params << bet << moveTime << gameTime << maxRating << minRating;

   // qDebug() << "moveTime.value " << moveTime.value
   // << " gameTime.value " << gameTime.value
   // << " minRating.value " << minRating.value
   // << " maxRating.value " << maxRating.value;

    connect(Client::instance(), SIGNAL(gameTableCreated(TABLEID)), this, SLOT(onGameTableCreated(TABLEID)));
    Client::instance()->createGameTable(LOGIC_ID_CHESS, params);
    MainWindow::instance()->setMode(MW_WAIT);
}


void CreateGameDialog::onExitClicked()
{
    MainWindow::instance()->showMainMenu();
}

void CreateGameDialog::onGameTableCreated(TABLEID id)
{
    disconnect(Client::instance(), SIGNAL(gameTableCreated(TABLEID)), this, SLOT(onGameTableCreated(TABLEID)));

    UI::instance()->setGameTable(id);
    MainWindow::instance()->showWaitJoinDialog(mGameTable);
}
