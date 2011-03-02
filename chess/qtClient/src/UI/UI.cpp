#include <assert.h>
#include <QDebug>
#include "UI.h"
#include "Exception.h"
#include "Client.h"
#include "MainWindow.h"
#include "Pixmaps.h"


UI::UI():
    mGameTable(0),
    mPlayerAuthotized(false)
{
}

UI::~UI()
{
  //  qDebug() << "UI::~UI()";
}

void UI::initialize(QApplication* app)
{
  //  qDebug() << "UI::initialize()";

    connect(Client::instance(), SIGNAL(error(const QString&)), this, SLOT(onError(const QString&)));

    MainWindow::instance()->initialize();

    mApp = app;

 //   qDebug() << "UI::initialize() ok";
}


void UI::shutdown()
{
 //   qDebug() << "UI::shutdown()";

   // delete MainWindow::instance();
 //   delete Client::instance();

    mApp->exit(0);
}



void UI::startGame()
{
    MainWindow::instance()->closeCurrentDialog();

    if(mPlayerColor == PC_WHITE)
    {
        mGameState = GS_WAIT_FOR_PLAYER_TOUCH;
    }
    else
    {
        mGameState = GS_WAIT_FOR_SERVER;
    }

    connect(Client::instance(), SIGNAL(invalidMove()), this, SLOT(onInvalidMove()));
    connect(Client::instance(), SIGNAL(gameOver(const QString&)), this, SLOT(onGameOver(const QString&)));
    connect(Client::instance(), SIGNAL(drawOffered()), this, SLOT(onDrawOffered()));

    assert(mGameTable);

    Client::instance()->getField(mGameTable);

    Client::instance()->joinTableChat(LOGIC_ID_CHESS, mGameTable);
}

void UI::surrender()
{
    TABLEID id = getGameTable();
    Client::instance()->surrender(id);
}


void UI::onGameOver(const QString& message)
{
    disconnect(Client::instance(), SIGNAL(invalidMove()), this, SLOT(onInvalidMove()));
    disconnect(Client::instance(), SIGNAL(gameOver(const QString&)), this, SLOT(onGameOver(const QString&)));
    disconnect(Client::instance(), SIGNAL(drawOffered()), this, SLOT(onDrawOffered()));



    MainWindow::instance()->closeCurrentDialog();
    MainWindow::instance()->showMessage(message);
    MainWindow::instance()->showMainMenu();
}


void UI::onInvalidMove()
{
    mGameState = GS_INVALID_MOVE;
}

void UI::onDrawOffered()
{
    disconnect(Client::instance(), SIGNAL(drawOffered()), this, SLOT(onDrawOffered()));

    bool accept = MainWindow::instance()->showQuestion(tr("Your opponent has offered a draw. Agree?"));

    Client::instance()->replyDraw(UI::instance()->getGameTable(), accept);

    connect(Client::instance(), SIGNAL(drawOffered()), this, SLOT(onDrawOffered()));
}

void UI::onError(const QString& what)
{
    disconnect(Client::instance(), SIGNAL(error(const QString&)), this, SLOT(onError(const QString&)));

 //   qDebug() << "UI::onError";
    MainWindow::instance()->closeCurrentDialog();
    MainWindow::instance()->showError(what);

    connect(Client::instance(), SIGNAL(error(const QString&)), this, SLOT(onError(const QString&)));
}


bool UI::isEnemyPiece(CELLID cell)
{
    bool res = false;
    if(mPlayerColor == PC_WHITE)
    {
        // qDebug() <<"isEnemyPiece" << cell << (mField.cells[cell] == PIX_BLACK_ROOK);

        res = (mField[cell] == b_Pawn) ||
              (mField[cell] == b_Rook) ||
              (mField[cell] == b_Knight) ||
              (mField[cell] == b_Bishop) ||
              (mField[cell] == b_Queen) ||
              (mField[cell] == b_King);
    }
    else if(mPlayerColor == PC_BLACK)
    {
        res = (mField[cell] == w_Pawn) ||
              (mField[cell] == w_Rook) ||
              (mField[cell] == w_Knight) ||
              (mField[cell] == w_Bishop) ||
              (mField[cell] == w_Queen) ||
              (mField[cell] == w_King);
    }

    return res;
}


void UI::cellClicked(CELLID cell)
{

    switch(mGameState)
    {
    case GS_WAIT_FOR_SERVER:

        // qDebug() << "No action - GS_WAIT_FOR_SERVER";
        return;

    case GS_WAIT_FOR_OPPONENT:

        // qDebug() << "No action - GS_WAIT_FOR_OPPONENT";
        return;

    case GS_WAIT_FOR_PLAYER_TOUCH:

        if(mField[cell] == Empty || isEnemyPiece(cell))
        {
            // qDebug() << "No action - Epmty field or Enemy piece";
            return;
        }
        else
        {
            MainWindow::instance()->highlightGameSceneCell(cell);

            mMove.srcCell = cell;
            mGameState = GS_WAIT_FOR_PLAYER_MOVE;
            // qDebug() << "OK! mSourceCell = " << cell;
        }

        break;

    case GS_WAIT_FOR_PLAYER_MOVE:

        if(mField[cell] == Empty || isEnemyPiece(cell))
        {
            mMove.dstCell = cell;
            // qDebug() << "OK! mDestinationCell = " << cell;

            MainWindow::instance()->highlightGameSceneCell(cell);

            // TODO Replace by UI::showNote, use QDialog
       //     mScene->showNote("Checking the move");
            assert(mGameTable);

            Client::instance()->step(mGameTable, mMove);

            // TODO send a move to server
            mGameState = GS_WAIT_FOR_SERVER;
        }
        else
        {
            MainWindow::instance()->removeGameSceneHighlight();
            MainWindow::instance()->highlightGameSceneCell(cell);

            mMove.srcCell = cell;
            mGameState = GS_WAIT_FOR_PLAYER_MOVE;
            // qDebug() << "OK! mSourceCell = " << cell;
        }

        break;

    default:
        THROW_EXCEPTION("Switch error");
        break;
    }



}


GameState UI::updateField(const Field& field, bool myMove, bool iAmWhite)
{
    mField = field;

    // pass the move
    mPlayerColor = iAmWhite ? PC_WHITE : PC_BLACK;

    mGameState = myMove ? GS_WAIT_FOR_PLAYER_TOUCH : GS_WAIT_FOR_OPPONENT;


    return mGameState;
}









