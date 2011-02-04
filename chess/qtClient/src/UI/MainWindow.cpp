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
#include "Dialogs/SendMessageDialog.h"
#include "Dialogs/OptionsDialog.h"
#include "Pixmaps.h"


MainWindow::MainWindow(QWidget *parent):
    QMainWindow(parent),
    mCurrentDialog(NULL),
    mMode(MW_NORMAL)
{
    qDebug() << "MainWindow::MainWindow";

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

    setCentralWidget(centralwidget);
    // centralwidget->showFullScreen(); // This may be redundant
    this->showFullScreen(); // This is TRUE! Tested on Meego Emulator.

    setWindowTitle(tr("Chess"));
    Q_UNUSED(this);

    QMetaObject::connectSlotsByName(this);

    connect(&mOrientation, SIGNAL(orientationChanged(OrientationStatus)), this, SLOT(onOrientationChanged(OrientationStatus)));
}

MainWindow::~MainWindow()
{
    qDebug() << "MainWindow::~MainWindow()";
}


void MainWindow::onOrientationChanged(OrientationStatus orientation)
{

    qDebug() << "onOrientationChanged: " << orientation;

    static bool initialized = false;

    if(!initialized)
    {
        // load items' properities
        int width = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_WIDTH).toInt();
        int height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_HEIGHT).toInt();
        int text_offset = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_BUTTONS << XML_NODE_TEXT_OFFSET).toInt();

        Button::setWidth(width);
        Button::setHeight(height);
        Button::setTextOffset(text_offset);

        int cellWidth = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_CELL << XML_NODE_WIDTH).toInt();
        Cell::setWidth(cellWidth);

        // load pixmaps
        Pixmaps::loadPixmaps();

        mMainMenu->initialize();
        mGameScene->initialize();

        showMainMenu();
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
    resize(mWidth, mHeight);


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
        hideCurrentDialog();
        setCursor(QCursor(Qt::WaitCursor));

        mMainMenu->disableItems();
    }
    else if(mode == MW_NORMAL)
    {
        setCursor(QCursor(Qt::ArrowCursor));
        mMainMenu->enableItems();
    }

    mMode = mode;
}



void MainWindow::setCurrentDialog(QDialog* dialog)
{
    deleteCurrentDialog();
    mCurrentDialog = dialog;

    // bind dialog to the current scene
    QGraphicsScene* curScene = mGraphicsView->scene();
    QGraphicsProxyWidget* proxy = curScene->addWidget(mCurrentDialog);
  //  proxy->setPos(curScene->width()/2  - mCurrentDialog->width()/2,
  //                curScene->height()/2 - mCurrentDialog->height()/ 2);

    proxy->setZValue(Z_DIALOG_LAYER);
}

void MainWindow::hideCurrentDialog()
{
    if(mCurrentDialog)
    {
        mCurrentDialog->hide();
    }
}

void MainWindow::deleteCurrentDialog()
{
    delete mCurrentDialog;
    mCurrentDialog = 0;
}



void MainWindow::showMessage(const QString& text)
{
    showMessageBox(QMessageBox::Information, tr("Message"), text);
}


void MainWindow::showError(const QString& text)
{
    qDebug() << "MainWindow::showError";
    showMessageBox(QMessageBox::Critical, tr("Error"), text);
}

bool MainWindow::showQuestion(const QString& text)
{
    if(showMessageBox(QMessageBox::Question, tr("Question"), text) == QMessageBox::Yes)
    {
        return true;
    }

    return false;
}

int MainWindow::showMessageBox(QMessageBox::Icon icon, const QString &title, const QString &text)
{
    qDebug() << "MainWindow::showMessageBox";
    setMode(MW_NORMAL);

    QMessageBox::StandardButtons buttons;
    if(icon == QMessageBox::Question)
    {
        buttons = QMessageBox::Yes | QMessageBox::No;
        qDebug() << "QUESTION:";
    }
    else
    {
        buttons = QMessageBox::Ok;
    }

    QMessageBox msgBox(icon, title, text, buttons, this);

    msgBox.show();
    msgBox.raise();
    msgBox.activateWindow();

    // a modal QMessageBox can't be added to a scene!
    msgBox.setModal(false);

    // prevent too wide messages
    msgBox.setFixedWidth(min(mGraphicsView->scene()->width(), mGraphicsView->scene()->height()));

    QGraphicsProxyWidget* proxy = mGraphicsView->scene()->addWidget(&msgBox);
    proxy->setZValue(Z_MESSAGE_LAYER);

    // simulate modality
    mMode = MW_WAIT;
    mMainMenu->disableItems();
    proxy->setOpacity(OPAQUE_NORMAL);

    int res = msgBox.exec();

    // clean-up
    mGraphicsView->scene()->removeItem(proxy);
    mMainMenu->enableItems();
    mMode = MW_NORMAL;

    return res;
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


void MainWindow::showJoinGameDialog(const QList<TABLEID>& tableIDs)
{
    setCurrentDialog(new JoinGameDialog(tableIDs, this));
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

void MainWindow::showSendMessageDialog(ChatType chatType)
{
    setCurrentDialog(new SendMessageDialog(chatType, this));
    setMode(MW_NORMAL);
}

void MainWindow::showMainMenu()
{

    deleteCurrentDialog();
    mGraphicsView->setScene(mMainMenu);

    show();

    setMode(MW_NORMAL); // TODO this should be in each show... function
}

void MainWindow::showGameScene(PlayerColor color)
{
    qDebug() << "MainWindow::showGameField";

    try
    {
        mGameScene->startClocks();

        mGraphicsView->setScene(mGameScene);

        show();
    }
    catch (Exception& e)
    {
        showMessage(e.what());
    }

}


void MainWindow::highlightGameSceneCell(CELLID cell)
{
    mGameScene->highlightCell(cell);
}

void MainWindow::removeGameSceneHighlight()
{
    mGameScene->removeHighlight();
}


