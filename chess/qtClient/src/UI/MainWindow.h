
#ifndef MAINWINDOW_H_
#define MAINWINDOW_H_


#include <QGraphicsView>
#include <QMainWindow>
#include <QVBoxLayout>
#include <QWidget>
#include <QDialog>

#include <GameScene.h>
#include <MainMenu.h>
#include <Defines.h>

#include "MyMessageBox.h"

class MainMenu;


enum MainWindowMode
{
    MW_NORMAL,
    MW_WAIT,
};

// This class is responsible for VISUALIZATION of the game
class MainWindow : public QMainWindow
{

    MainWindow(QWidget *parent = 0): QMainWindow(parent) {}

public:

    ~MainWindow() {}

    static MainWindow* instance()
    {
        static MainWindow* mainWindow = new MainWindow();
        return mainWindow;
    }


    void initialize();

    // dialogs
    void showAuthorizationDialog();
    void showCreateGameDialog();
    void showFindGameDialog();
    void showWalletDialog();
    void showJoinGameDialog(const QList<GameTable>& tables);
    void showGameDialog(const QString& text);
    void showOptionsDialog();
    void showChatMessageDialog(const QString& addressee, ChatType chatType);
    piece_type showPromotionDialog(PlayerColor color);

    void closeCurrentDialog();

    // scenes
    void showMainMenu();
    void showGameScene();
    void updateGameScene();

    // TODO create 1 method instead 4
    void showWaitJoinDialog(const GameTable& gameTable);
    void showWaitAgreeDialog();
    void showWaitDrawDialog();


    // should be called only if dialogs are hidden
    void showError(const QString& text);
    void showMessage(const QString& text);
    bool showQuestion(const QString& text);

    void setMode(MainWindowMode mode);
    MainWindowMode getMode() { return mMode; }

    int width() { return mWidth; }
    int height() { return mHeight; }

    void setWidth(int width) { mWidth = width; }
    void setHeight(int height) { mHeight = height; }

private:


    void setCurrentDialog(QDialog* dialog);
    int showMessageBox(MyMessageBoxType type, const QString &text);

    QWidget *centralwidget;
    QVBoxLayout *vboxLayout;
    QGraphicsView *mGraphicsView;

    QDialog*  mCurrentDialog;

    MainMenu* mMainMenu;
    GameScene* mGameScene;

    MainWindowMode mMode;

    int mWidth;
    int mHeight;


};


#endif /* MAINWINDOW_H_ */
