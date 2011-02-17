/*
 * MyMessageBox.cpp
 *
 *  Created on: Feb 17, 2011
 *      Author: ssy
 */

#include "MyMessageBox.h"
#include <QDebug>

MyMessageBox::MyMessageBox(QWidget *parent, MyMessageBoxType type, const QString &text):
    MyDialog(parent)
{
    label = new QLabel(text, this);

   // upperLayout = new QVBoxLayout();
    layout = new QVBoxLayout(this);
    lowerLayout = new QHBoxLayout();

    layout->addWidget(label);
    //upperLayout->addStretch();


    switch(type)
    {
        case MB_OK:
        case MB_ERROR:
            okButton = new QPushButton(tr("OK"), this);
            lowerLayout->addWidget(okButton);
            connect(okButton, SIGNAL(clicked()), this, SLOT(onOkClicked()));
            break;

        case MB_QUESTION:
            yesButton = new QPushButton(tr("Yes"), this);
            noButton = new QPushButton(tr("No"), this);

            lowerLayout->addWidget(yesButton);
            lowerLayout->addWidget(noButton);

            connect(yesButton, SIGNAL(clicked()), this, SLOT(onYesClicked()));
            connect(noButton, SIGNAL(clicked()), this, SLOT(onNoClicked()));
            break;

        default:
            break;
    }


   // layout->addLayout(upperLayout);
    layout->addLayout(lowerLayout);

    this->setLayout(layout);
}

MyMessageBox::~MyMessageBox() {
    // TODO Auto-generated destructor stub
}

void MyMessageBox::onOkClicked()
{
    disconnect(okButton, SIGNAL(clicked()), this, SLOT(onOkClicked()));
    mRetCode =  QDialog::Accepted;
    close();
}

void MyMessageBox::onYesClicked()
{
    disconnect(yesButton, SIGNAL(clicked()), this, SLOT(onYesClicked()));
    mRetCode = QDialog::Accepted;
    close();
}

void MyMessageBox::onNoClicked()
{

    disconnect(noButton, SIGNAL(clicked()), this, SLOT(onNoClicked()));
    mRetCode =  QDialog::Rejected;
    close();
}
