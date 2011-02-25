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
#include <Clock.h>
#include <CaptureBox.h>
#include <orientation.h>


class GameScene: public QGraphicsScene
{
    Q_OBJECT
public:


    GameScene(QObject *parent = 0);

    ~GameScene();

    void initialize();

    void startClocks();
    void showChat();

    void highlightCell(CELLID cell);

    void removeHighlight();

    void updateItemsPositions(OrientationStatus orientation);

    void hide();

private:

    void updateClocks();
    void updateGameField(const Field& field, bool white);
    void setGameStateText(GameState gameState);

    QObject *mParent;

    QGraphicsRectItem* mBoard;
    QGraphicsItem* mCells;  // group of cells
    vector <Cell*> mCellArray;

    QGraphicsItem* mPieces; // group of pieces
    QGraphicsItem* mHighlights; // group of Highlights
    QGraphicsItem* mNote;
    QGraphicsTextItem* mGameStateText;
    QGraphicsTextItem* mTimeText;

    Button* mMenuButton;
    Button* mExitButton;

    CELLID mHighlightedCell;

    void loadImages();

    Chat* mChat;

    Clock mMoveClock;
    Clock mGameClock;

    Field mField;
    bool mWhite;

    CaptureBox mCaptureBox;

private slots:

    void onGotField(const Field& field, bool myMove, bool iAmWhite);
    void onMenuButtonClicked();
    void onCellClicked(CELLID cellID);
    void onExitClicked();

};


#endif /* SCENE_H_ */
