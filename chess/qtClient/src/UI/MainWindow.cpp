/*
 * MainWindow.cpp
 *
 *  Created on: Apr 5, 2010
 *      Author: ssy
 */

#include <QApplication>
#include <QDebug>
#include <QMessageBox>
#include <QProgressDialog>
#include <QGraphicsProxyWidget>

#include "MainWindow.h"
#include "Client.h"
#include "UI.h"
#include "Exception.h"
#include <Defines.h>
#include <XML.h>
#include <math.h>


#include "Dialogs/AuthorizationDialog.h"
#include "Dialogs/CreateGameDialog.h"
#include "Dialogs/JoinGameDialog.h"
#include "Dialogs/FindGameDialog.h"
#include "Dialogs/GameDialog.h"
#include "Dialogs/WaitJoinDialog.h"
#include "Dialogs/WaitAgreeDialog.h"
#include "Dialogs/WaitDrawDialog.h"
#include "Dialogs/ChatMessageDialog.h"
#include "Dialogs/OptionsDialog.h"
#include "Dialogs/PromotionDialog.h"
#include "Pixmaps.h"


MainWindow::MainWindow(QWidget *parent):
    QMainWindow(parent),
    mCurrentDialog(NULL),
    mMode(MW_NORMAL)
{
  //  qDebug() << "MainWindow::MainWindow";

    mGameScene = new GameScene(this);
    mMainMenu = new MainMenu(this);

    centralwidget = new QWidget(this);

    QString color = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_WINDOW << XML_NODE_COLOR);
    centralwidget->setPalette(QPalette(QColor(color)));

    /*
    qDebug() << "Window Geometry:";
    qDebug() << this->geometry().width();
    qDebug() << this->geometry().height();
*/

    vboxLayout    = new QVBoxLayout(centralwidget);
    mGraphicsView  = new QGraphicsView(centralwidget);

    vboxLayout->addWidget(mGraphicsView);
    vboxLayout->setContentsMargins(0,0,0,0); // no margins around the scene

    setCentralWidget(centralwidget);
    // centralwidget->showFullScreen(); // This may be redundant
   // this->showFullScreen(); // This is TRUE! Tested on Meego Emulator.

    setWindowTitle(tr("Chess"));
    Q_UNUSED(this);

    QMetaObject::connectSlotsByName(this);

    connect(&mOrientation, SIGNAL(orientationChanged(OrientationStatus)), this, SLOT(onOrientationChanged(OrientationStatus)));
}

MainWindow::~MainWindow()
{
   // qDebug() << "MainWindow::~MainWindow()";
}


void MainWindow::onOrientationChanged(OrientationStatus orientation)
{

   // qDebug() << "onOrientationChanged: " << orientation;

    static bool initialized = false;

    if(!initialized)
    {
        // load pixmaps
        Pixmaps::loadPixmaps();

        mMainMenu->initialize();
        mGameScene->initialize();

        showMainMenu();

        show();
    }

    QString orientNode = (orientation == OrientationHorizontal) ? XML_NODE_LANDSCAPE : XML_NODE_PORTRAIT;

    // Rotation
    if(orientation == OrientationVertical)
    {
        mGraphicsView->rotate(-90); // degrees
    }

    if(initialized && orientation == OrientationHorizontal)
    {
        mGraphicsView->rotate(90); // degrees
    }

    // Resize
    QList<QString> path;
    mWidth = XML::instance().readValue(XML_ITEMS_FILENAME, path << XML_NODE_MAIN_WINDOW << orientNode << XML_NODE_WIDTH).toInt(); path.clear();
    mHeight = XML::instance().readValue(XML_ITEMS_FILENAME, path << XML_NODE_MAIN_WINDOW << orientNode << XML_NODE_HEIGHT).toInt(); path.clear();
    setFixedSize(mWidth, mHeight);


    mMainMenu->updateItemsPositions(orientation);
    mGameScene->updateItemsPositions(orientation);

    initialized = true;
}


void MainWindow::initialize()
{
    // the initialization was moved into 'onOrientationChanged'
    //onOrientationChanged(mOrientation.getActualOrientation());
    onOrientationChanged(OrientationHorizontal);
    setMode(MW_NORMAL);
}


void MainWindow::setMode(MainWindowMode mode)
{
    if(mode == MW_WAIT)
    {
        closeCurrentDialog();
        setCursor(QCursor(Qt::WaitCursor));

        mMainMenu->disableItems();
        mGameScene->disableItems();
    }
    else if(mode == MW_NORMAL)
    {
        setCursor(QCursor(Qt::ArrowCursor));
        mMainMenu->enableItems();
        mGameScene->enableItems();
    }

    mMode = mode;
}



void MainWindow::setCurrentDialog(QDialog* dialog)
{
    closeCurrentDialog();
    mCurrentDialog = dialog;



    // bind dialog to the current scene
    QGraphicsScene* curScene = mGraphicsView->scene();
  //  QGraphicsProxyWidget* proxy = curScene->addWidget(mCurrentDialog);
  //  proxy->setPos(curScene->width()/2  - mCurrentDialog->width()/2,
 //                 curScene->height()/2 - mCurrentDialog->height()/ 2);

  //  proxy->setZValue(Z_DIALOG_LAYER);

    mCurrentDialog->show();
}


void MainWindow::closeCurrentDialog()
{
    if(mCurrentDialog)
    {
        mCurrentDialog->close();
    }
}



void MainWindow::showMessage(const QString& text)
{
    showMessageBox(MB_OK, text);
}


void MainWindow::showError(const QString& text)
{
   // qDebug() << "MainWindow::showError";
    showMessageBox(MB_ERROR, text);
}

bool MainWindow::showQuestion(const QString& text)
{
    if(showMessageBox(MB_QUESTION, text) == QDialog::Accepted)
    {
        return true;
    }

    return false;
}

int MainWindow::showMessageBox(MyMessageBoxType type, const QString &text)
{
    setMode(MW_NORMAL);

    MyMessageBox msgBox(this, type, text);

    msgBox.exec();

    return msgBox.retCode();
}



void MainWindow::showAuthorizationDialog()
{
    setCurrentDialog(new AuthorizationDialog(this));
    setMode(MW_NORMAL);
}

void MainWindow::showCreateGameDialog()
{
    setCurrentDialog(new CreateGameDialog(this));
    setMode(MW_NORMAL);
}

void MainWindow::showFindGameDialog()
{
    setCurrentDialog(new FindGameDialog(this));
    setMode(MW_NORMAL);
}


void MainWindow::showJoinGameDialog(const QList<GameTable>& tables)
{
    setCurrentDialog(new JoinGameDialog(tables, this));
    setMode(MW_NORMAL);
}

void MainWindow::showGameDialog()
{
    setCurrentDialog(new GameDialog(this));
    setMode(MW_NORMAL);
}

void MainWindow::showOptionsDialog()
{
    setCurrentDialog(new OptionsDialog(this));
    setMode(MW_NORMAL);
}

void MainWindow::showWaitDrawDialog()
{
  //  qDebug() << "MainWindow::showWaitDrawDialog";
    setCurrentDialog(new WaitDrawDialog(this));
    setMode(MW_NORMAL);
}

void MainWindow::showWaitJoinDialog()
{
    setCurrentDialog(new WaitJoinDialog(this));
    setMode(MW_NORMAL);
}

void MainWindow::showWaitAgreeDialog()
{
    setCurrentDialog(new WaitAgreeDialog(this));
    setMode(MW_NORMAL);
}

void MainWindow::showChatMessageDialog(const QString& addressee, ChatType chatType)
{
    setCurrentDialog(new ChatMessageDialog(addressee, chatType, this));
    setMode(MW_NORMAL);
}

piece_type MainWindow::showPromotionDialog(PlayerColor color)
{
    PromotionDialog* dialog = new PromotionDialog(color, this);

    return (piece_type)dialog->exec();
}

void MainWindow::showMainMenu()
{
  //  if(mGraphicsView->scene() == mGameScene)
    if(mGameScene)
        mGameScene->close();

    closeCurrentDialog();
    mGraphicsView->setScene(mMainMenu);

    setMode(MW_NORMAL); // TODO this should be in each show... function
}

void MainWindow::showGameScene(PlayerColor color)
{
   // qDebug() << "MainWindow::showGameScene";
    if(mGraphicsView->scene() == mGameScene)
        return;

    if(mGraphicsView->scene() == mMainMenu)
        mMainMenu->close();

    mGameScene->showChat();
    mGameScene->startClocks();

    mGraphicsView->setScene(mGameScene);

    setMode(MW_NORMAL);

}



void MainWindow::highlightGameSceneCell(CELLID cell)
{
    mGameScene->highlightCell(cell);
}

void MainWindow::removeGameSceneHighlight()
{
    mGameScene->removeHighlight();
}

void MainWindow::enableGameSceneAnimation(const Move& move)
{
    mGameScene->enableAnimation(move);
}

void MainWindow::disableGameSceneAnimation()
{
    mGameScene->disableAnimation();
}


