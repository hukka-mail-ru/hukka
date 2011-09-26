/*
 * Cell.h
 *
 *  Created on: May 20, 2010
 *      Author: ssy
 */

#ifndef CELL_H_
#define CELL_H_
#include "Button.h"
#include <Global.h>
#include <Pixmaps.h>
#include <QObject>

class Cell: public Button
{
    Q_OBJECT
public:
    Cell(QGraphicsScene* scene, CELLID cellID, PixmapKey cellPixmapKey, QObject* parent = 0);

    void setPiece(piece_type pieceType);
    piece_type getPiece() { return mPieceType; }

    PixmapKey getPieceKey() { return mPiecePixmapKey; }

    // for animation
    void showPiece();
    void hidePiece();

    void highlight(HighlightColor color = HC_WHITE);
    void removeHighlight();

    int width() { return Pixmaps::get(mCellPixmapKey).width(); }

signals:
    void cellClicked(CELLID id);

protected:

   virtual void mousePressEvent (QGraphicsSceneMouseEvent * event);
   virtual void mouseReleaseEvent (QGraphicsSceneMouseEvent * event);

private:

    void applyPixmap(PixmapKey piecePixmapKey);

    CELLID mId;
    PixmapKey mCellPixmapKey;
    PixmapKey mPiecePixmapKey;

    bool mIsPiece;
    bool mIsHighlight;

    piece_type mPieceType;

    PixmapKey mHighlight;
};

#endif /* CELL_H_ */
