/*
 * PromotionDialog.h
 *
 *  Created on: Mar 24, 2011
 *      Author: ssy
 */

#ifndef PROMOTIONDIALOG_H_
#define PROMOTIONDIALOG_H_

#include <QLabel>
#include <QPushButton>
#include <QVBoxLayout>
#include <QHBoxLayout>

#include <MyDialog.h>
#include <Defines.h>

class PromotionDialog: public MyDialog
{
    Q_OBJECT
public:
    PromotionDialog(PlayerColor color, QWidget *parent = 0);
    virtual ~PromotionDialog();

private:

    QPushButton* newButton(const QPixmap& pixmap, const char* slot);

    QLabel* label;

    QPushButton* queenButton;
    QPushButton* rookButton;
    QPushButton* bishopButton;
    QPushButton* knightButton;

    QVBoxLayout* upperLayout;
    QHBoxLayout* lowerLayout;
    QVBoxLayout* layout;

private slots:

    void onQueenClicked();
    void onRookClicked();
    void onBishopClicked();
    void onKnightClicked();

};

#endif /* PROMOTIONDIALOG_H_ */
