/*
 * CaptureBox.h
 *
 *  Created on: Feb 25, 2011
 *      Author: ssy
 */

#ifndef CAPTUREBOX_H_
#define CAPTUREBOX_H_

#include <QGraphicsScene>
#include <QGraphicsRectItem>
#include <QString>

#include <Pixmaps.h>

class CaptureBox {
public:
    CaptureBox(QGraphicsScene* parentScene);
    virtual ~CaptureBox();

    void update(const Field& field, bool white);
private:
    QGraphicsScene* mParentScene;

    int mMeX;
    int mMeY;
    int mMeWidth;
    int mMeHeight;

    int mOpponentX;
    int mOpponentY;
    int mOpponentWidth;
    int mOpponentHeight;

    int mBorderWidth;

    QString mBgForWhitesColor;
    QString mBgForBlacksColor;

    QGraphicsRectItem* mMeBox;
    QGraphicsRectItem* mOppBox;

    int mCapturedPieceWidth;
};

#endif /* CAPTUREBOX_H_ */
