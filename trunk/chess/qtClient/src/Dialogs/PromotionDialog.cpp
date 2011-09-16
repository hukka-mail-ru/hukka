/*
 * PromotionDialog.cpp
 *
 *  Created on: Mar 24, 2011
 *      Author: ssy
 */
#include <QDebug>

#include <XML.h>
#include <Pixmaps.h>

#include "PromotionDialog.h"

QPushButton* PromotionDialog::newButton(const QPixmap& pixmap, const char* slot)
{
    QPushButton* button = new QPushButton();

    button->setIcon(QIcon(pixmap));

    int height = XML::instance().readValue(XML_ITEMS_FILENAME, QList<QString>() << XML_NODE_PROMOTION << XML_NODE_HEIGHT).toInt() * 2;
    button->setMinimumHeight(height);
    button->setIconSize(QSize(pixmap.width(),pixmap.height()));

    QObject::connect(button, SIGNAL(clicked()), this, slot);
    return button;
}


PromotionDialog::PromotionDialog(PlayerColor color, QWidget *parent):
    MyDialog(parent)
{
    if(color == PC_WHITE)
    {
        queenButton  = newButton(Pixmaps::get(PIX_PROMOTION_WHITE_QUEEN),  SLOT(onQueenClicked()));
        rookButton   = newButton(Pixmaps::get(PIX_PROMOTION_WHITE_ROOK),   SLOT(onRookClicked()));
        bishopButton = newButton(Pixmaps::get(PIX_PROMOTION_WHITE_BISHOP), SLOT(onBishopClicked()));
        knightButton = newButton(Pixmaps::get(PIX_PROMOTION_WHITE_KNIGHT), SLOT(onKnightClicked()));
    }
    else if(color == PC_BLACK)
    {
        queenButton  = newButton(Pixmaps::get(PIX_PROMOTION_BLACK_QUEEN),  SLOT(onQueenClicked()));
        rookButton   = newButton(Pixmaps::get(PIX_PROMOTION_BLACK_ROOK),   SLOT(onRookClicked()));
        bishopButton = newButton(Pixmaps::get(PIX_PROMOTION_BLACK_BISHOP), SLOT(onBishopClicked()));
        knightButton = newButton(Pixmaps::get(PIX_PROMOTION_BLACK_KNIGHT), SLOT(onKnightClicked()));
    }


    label = new QLabel(tr("Please choose the promotion:"), this);

    upperLayout = new QVBoxLayout();
    upperLayout->addWidget(label);
    upperLayout->addStretch();

    lowerLayout = new QHBoxLayout();
    lowerLayout->addWidget(queenButton);
    lowerLayout->addWidget(rookButton);
    lowerLayout->addWidget(bishopButton);
    lowerLayout->addWidget(knightButton);

    layout = new QVBoxLayout(this);
    layout->addLayout(upperLayout);
    layout->addLayout(lowerLayout);

    this->setLayout(layout);
    this->setModal(true);
}



PromotionDialog::~PromotionDialog() {
    // TODO Auto-generated destructor stub
}


void PromotionDialog::onQueenClicked()
{
    qDebug() << "onQueenClicked";
    done(Queen);
}

void PromotionDialog::onRookClicked()
{
    qDebug() << "onRookClicked";
    done(Rook);
}

void PromotionDialog::onBishopClicked()
{
    qDebug() << "onBishopClicked";
    done(Bishop);
}

void PromotionDialog::onKnightClicked()
{
    qDebug() << "onKnightClicked";
    done(Knight);
}
