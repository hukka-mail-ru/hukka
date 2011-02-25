/*
 * CaptureBox.h
 *
 *  Created on: Feb 25, 2011
 *      Author: ssy
 */

#ifndef CAPTUREBOX_H_
#define CAPTUREBOX_H_

#include <QGraphicsScene>

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
    int mOpponentX;
    int mOpponentY;
    int mWidth;
};

#endif /* CAPTUREBOX_H_ */
