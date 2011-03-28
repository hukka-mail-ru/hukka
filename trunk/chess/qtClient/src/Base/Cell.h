/*
 * Cell.h
 *
 *  Created on: May 20, 2010
 *      Author: ssy
 */

#ifndef CELL_H_
#define CELL_H_
#include "Button.h"
#include <Defines.h>
#include <Pixmaps.h>
#include <QObject>

class Cell: public Button
{
    Q_OBJECT
public:
    Cell(QGraphicsScene* scene, CELLID cellID, PixmapKey cellPixmapKey, QObject* parent = 0);

    void setPiece(piece_type pieceType);
    PixmapKey getPieceKey() { return mPiecePixmapKey; }

    // for animation
    void showPiece(PixmapKey pieceKey);
    void hidePiece();

    void highlight();
    void removeHighlight();

    static int width() { return mWidth; }
    static void setWidth(int width) { mWidth = width; }

signals:
    void cellClicked(CELLID id);

protected:

   virtual void mousePressEvent (QGraphicsSceneMouseEvent * event);
   virtual void mouseReleaseEvent (QGraphicsSceneMouseEvent * event);

private:

    void applyPixmap(PixmapKey piecePixmapKey);

    static int mWidth;

    CELLID mId;
    PixmapKey mCellPixmapKey;
    PixmapKey mPiecePixmapKey;

    bool mIsPiece;
    bool mIsHighlight;

};

#endif /* CELL_H_ */
