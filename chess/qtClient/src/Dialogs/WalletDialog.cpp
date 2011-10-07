/*
 * WalletDialog.cpp
 *
 *  Created on: Oct 7, 2011
 *      Author: ssy
 */

#include "WalletDialog.h"
#include <UI.h>

WalletDialog::WalletDialog(QWidget *parent):
    MyDialog(parent)
{
    // TODO Auto-generated constructor stub
    label = new QLabel(tr("You have now ") + QString::number(UI::instance()->getPlayer(PT_ME).balance) +
                       tr(" RUR in your wallet. ") + "\n\n" +
                       tr("To replenish your wallet please enter your PIN and press 'OK'. "));
    label->setWordWrap(true);

    exitButton = new QPushButton(tr("Exit"), this);
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));

    okButton = new QPushButton(tr("Ok"), this);
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));

    pinEdit = new QLineEdit("", this);

    QHBoxLayout* pinLayout = new QHBoxLayout();
    QLabel* pinLabel = new QLabel(tr("PIN:"));
    pinLayout->addWidget(pinLabel);
    pinLayout->addWidget(pinEdit);

    QLabel* obtainLabel = new QLabel("\n" + tr("If you have no PIN, you can obtain it here: money.yandex.ru"));
    obtainLabel->setWordWrap(true);

    upperLayout = new QVBoxLayout();
    upperLayout->addWidget(label);
    upperLayout->addLayout(pinLayout);
    upperLayout->addWidget(obtainLabel);
    upperLayout->addStretch();

    lowerLayout = new QHBoxLayout();
    lowerLayout->addWidget(okButton);
    lowerLayout->addWidget(exitButton);

    layout = new QVBoxLayout(this);
    layout->addLayout(upperLayout);
    layout->addLayout(lowerLayout);

    this->setLayout(layout);

}

WalletDialog::~WalletDialog() {
    // TODO Auto-generated destructor stub
}


void WalletDialog::onOkClicked()
{
}

void WalletDialog::onExitClicked()
{
    close();
}
