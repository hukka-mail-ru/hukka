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
#include <CaptureBox.h>
#include <orientation.h>
#include <MoveBox.h>
#include <Animation.h>



class GameScene: public QGraphicsScene
{
    Q_OBJECT
public:


    GameScene(QObject *parent = 0);

    ~GameScene();

    void initialize();

    void enableItems();
    void disableItems();

    void showChat();
    void startClocks();

    void highlightCell(CELLID cell);
    void removeHighlight();
    void enableAnimation(const Move& move);
    void disableAnimation();

    void updateItemsPositions(OrientationStatus orientation);

    void close();

    void repaintCells() { for(int i =0; i<mCellArray.size(); i++) mCellArray[i]->showPiece(); }

private:

    void updateGameField(const Field& field, bool white);
    void updateMoveBoxes(GameState gameState);

    QObject *mParent;

    QGraphicsRectItem* mBoard;
    int mBoardX;
    int mBoardY;

    QGraphicsItem* mCells;  // group of cells
    vector <Cell*> mCellArray;

    QGraphicsItem* mPieces; // group of pieces
    QGraphicsItem* mHighlights; // group of Highlights
    QGraphicsItem* mNote;

    Button* mMenuButton;
    Button* mExitButton;

    CELLID mHighlightedCell;

    void loadImages();

    Chat* mChat;

    MoveBox mMeMoveBox;
    MoveBox mOppMoveBox;

    Field mField;
    bool mWhite;

    CaptureBox mCaptureBox;

    Animation mAnimation;

private slots:

    void onGotField(const Field& field, bool myMove, bool iAmWhite);
    void onMenuButtonClicked();
    void onCellClicked(CELLID cellID);
    void onExitClicked();

};


#endif /* SCENE_H_ */
