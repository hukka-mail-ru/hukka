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
#include "Dialogs/WalletDialog.h"
#include "Pixmaps.h"



void MainWindow::initialize()
{
    mCurrentDialog = NULL;

    // MAIN WINDOW BASICS
    centralwidget = new QWidget(this);

    QString color = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_MAIN_WINDOW << XML_NODE_COLOR);
    centralwidget->setPalette(QPalette(QColor(color)));

    vboxLayout    = new QVBoxLayout(centralwidget);
    mGraphicsView  = new QGraphicsView(centralwidget);
    mGraphicsView->setFrameShape(QFrame::NoFrame);

    vboxLayout->addWidget(mGraphicsView);
    vboxLayout->setContentsMargins(0,0,0,0); // no margins around the scene

    setCentralWidget(centralwidget);
    // centralwidget->showFullScreen(); // This may be redundant
   // this->showFullScreen(); // This is TRUE! Tested on Meego Emulator.

    // Resize
    QList<QString> path;
    mWidth = XML::instance().readValue(XML_ITEMS_FILENAME, path << XML_NODE_MAIN_WINDOW << XML_NODE_WIDTH).toInt(); path.clear();
    mHeight = XML::instance().readValue(XML_ITEMS_FILENAME, path << XML_NODE_MAIN_WINDOW << XML_NODE_HEIGHT).toInt(); path.clear();
    setFixedSize(mWidth, mHeight);

    setWindowTitle(tr("Good Old Chess"));


     // PIXMAPS
     Pixmaps::loadPixmaps();


     // SCENES
     mGameScene = new GameScene(this);
     mMainMenu = new MainMenu(this);


     // OK, LET'S ROCK!
     showMainMenu();
     show();

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
    delete mCurrentDialog;

    mCurrentDialog = dialog;


    // bind dialog to the current scene
    QGraphicsScene* curScene = mGraphicsView->scene();

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

void MainWindow::showWalletDialog()
{
    setCurrentDialog(new WalletDialog(this));
    setMode(MW_NORMAL);
}


void MainWindow::showJoinGameDialog(const QList<GameTable>& tables)
{
    setCurrentDialog(new JoinGameDialog(tables, this));
    setMode(MW_NORMAL);
}

void MainWindow::showGameDialog(const QString& text)
{
    setCurrentDialog(new GameDialog(text, this));
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

void MainWindow::showWaitJoinDialog(const GameTable& gameTable)
{
    setCurrentDialog(new WaitJoinDialog(gameTable, this));
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

void MainWindow::showGameScene()
{
   // qDebug() << "MainWindow::showGameScene";
    if(mGraphicsView->scene() == mGameScene)
        return;

    if(mGraphicsView->scene() == mMainMenu)
        mMainMenu->close();

    mGameScene->showChat();

    mGraphicsView->setScene(mGameScene);

    setMode(MW_NORMAL);

}
void MainWindow::updateGameScene()
{
    if(mGraphicsView->scene() != mGameScene)
        return;

    mGameScene->update();
}



