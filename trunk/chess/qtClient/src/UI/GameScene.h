#ifndef GAME_SCENE_H_
#define GAME_SCENE_H_

#include <QGraphicsSceneMouseEvent>
#include <QGraphicsScene>
#include <QGraphicsPixmapItem>
#include <QGraphicsItemGroup>
#include <Defines.h>
#include <Button.h>
#include <Cell.h>
#include <Chat.h>
#include <MoveBox.h>
#include <Board.h>



class GameScene: public QGraphicsScene
{
    Q_OBJECT
public:


    GameScene(QObject *parent = 0);
    ~GameScene() {}

    void update();

    void enableItems();
    void disableItems();

    void showChat();
    void closeChat();

    void close();


private:

    void updateMoveBoxes(GameState gameState);

    Button* mGameMenuButton;
    Button* mChatButton;

    Chat* mChat;

    MoveBox mMoveBox;

    QGraphicsPixmapItem* mChatBackground;

    Board mBoard;

private slots:

    void onGotPosition(const Position& position);
    void onInvalidMove();
    void onMenuButtonClicked();
    void onChatButtonClicked();
 //   void onExitClicked();

};


#endif /* SCENE_H_ */
