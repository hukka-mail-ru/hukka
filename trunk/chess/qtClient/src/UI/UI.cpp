#include <assert.h>
#include <QDebug>
#include "UI.h"
#include "Exception.h"
#include "Client.h"
#include "MainWindow.h"
#include "Pixmaps.h"


UI::UI():
    mGameTable(0)
{
    mMe.isAuthorized = false;
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
    // Client must properly send all possible messages in its queue.
    connect(Client::instance(), SIGNAL(disconnectedFromHost()), this, SLOT(onDisconnected()));
    Client::instance()->disconnectFromHost();
}

void UI::onDisconnected()
{
    disconnect(Client::instance(), SIGNAL(disconnectedFromHost()), this, SLOT(onDisconnected()));

    mApp->exit(0);
}


void UI::startGame()
{
    MainWindow::instance()->closeCurrentDialog();

    if(mMe.color == PC_WHITE)
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


    Client::instance()->deleteLastGameResult();

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
    if(mMe.color == PC_WHITE)
    {
        // qDebug() <<"isEnemyPiece" << cell << (mField.cells[cell] == PIX_BLACK_ROOK);

        res = (mField[cell] == b_Pawn) ||
              (mField[cell] == b_Rook) ||
              (mField[cell] == b_Knight) ||
              (mField[cell] == b_Bishop) ||
              (mField[cell] == b_Queen) ||
              (mField[cell] == b_King);
    }
    else if(mMe.color == PC_BLACK)
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

piece_type getPromotion(piece_type piece, CELLID dstCell)
{
    piece_type res = Empty;

    if(piece != w_Pawn && piece != b_Pawn)
        return res;

    if(piece == w_Pawn && dstCell >= (CELLS_IN_FIELD - CELLS_IN_ROW)) // the last line for whites
    {
 //       qDebug() << "getPromotion 1";
        res = MainWindow::instance()->showPromotionDialog(PC_WHITE);
    }
    else if(piece == b_Pawn && dstCell < CELLS_IN_ROW) // the last line for blacks
    {
 //       qDebug() << "getPromotion 2";
        res = MainWindow::instance()->showPromotionDialog(PC_BLACK);
    }

  //  qDebug() << "getPromotion 3";

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
            mMove.srcCell = cell;

            MainWindow::instance()->highlightGameSceneCell(mMove.srcCell);
            mGameState = GS_WAIT_FOR_PLAYER_MOVE;
            // qDebug() << "OK! mSourceCell = " << cell;
        }

        break;

    case GS_WAIT_FOR_PLAYER_MOVE:

        if(mField[cell] == Empty || isEnemyPiece(cell))
        {
            mMove.dstCell = cell;
            // qDebug() << "OK! mDestinationCell = " << cell;

            MainWindow::instance()->highlightGameSceneCell(mMove.dstCell);
            MainWindow::instance()->enableGameSceneAnimation(mMove);

         //   qDebug() << "mField[mMove.srcCell]" << mField[mMove.srcCell];
         //   qDebug() << "mMove.dstCell" << mMove.dstCell;

            piece_type promotion = getPromotion(mField[mMove.srcCell], mMove.dstCell);

            Client::instance()->move(mGameTable, mMove, promotion);

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
    mMe.color = iAmWhite ? PC_WHITE : PC_BLACK;
    mOpponent.color = iAmWhite ? PC_BLACK : PC_WHITE;

    mGameState = myMove ? GS_WAIT_FOR_PLAYER_TOUCH : GS_WAIT_FOR_OPPONENT;


    return mGameState;
}


void UI::setPlayerName(PlayerType type, const QString& name)
{
    switch(type)
    {
        case PT_ME:       mMe.name = name; break;
        case PT_OPPONENT: mOpponent.name = name; break;
        default: break;
    }
}

void UI::setPlayerRating(PlayerType type, unsigned rating)
{
    switch(type)
    {
        case PT_ME:       mMe.rating = rating; break;
        case PT_OPPONENT: mOpponent.rating = rating; break;
        default: break;
    }
}
void UI::setPlayerColor(PlayerType type, PlayerColor color)
{
    switch(type)
    {
        case PT_ME:       mMe.color = color; break;
        case PT_OPPONENT: mOpponent.color = color; break;
        default: break;
    }
}







