
#ifndef MAINWINDOW_H_
#define MAINWINDOW_H_


#include <QGraphicsView>
#include <QMainWindow>
#include <QVBoxLayout>
#include <QWidget>
#include <QDialog>

#include <Scenes/GameScene.h>
#include <Scenes/MainMenu.h>
#include <Defines.h>
#include <orientation.h>

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
    Q_OBJECT

    MainWindow(QWidget *parent = 0);

public:

    ~MainWindow();

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
    void showJoinGameDialog(const QList<GameTable>& tables);
    void showGameDialog();
    void showOptionsDialog();
    void showChatMessageDialog(const QString& addressee, ChatType chatType);
    piece_type showPromotionDialog(PlayerColor color);

    void closeCurrentDialog();

    // scenes
    void showMainMenu();
    void showGameScene(PlayerColor color);

    QRectF sceneRect();

    // TODO create 1 method instead 4
    void showWaitJoinDialog();
    void showWaitAgreeDialog();
    void showWaitDrawDialog();


    void highlightGameSceneCell(CELLID cell);
    void removeGameSceneHighlight();
    void enableGameSceneAnimation(const Move& move);
    void disableGameSceneAnimation();

    // should be called only if dialogs are hidden
    void showError(const QString& text);
    void showMessage(const QString& text);
    bool showQuestion(const QString& text);

    void setMode(MainWindowMode mode);
    MainWindowMode getMode() { return mMode; }

    OrientationStatus getOrientation() { return mOrientation.getActualOrientation(); }

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

    Orientation mOrientation;

    MainWindowMode mMode;

    int mWidth;
    int mHeight;

private slots:

    void onOrientationChanged(OrientationStatus orientation);

};


#endif /* MAINWINDOW_H_ */
