/*
 * MyMessageBox.h
 *
 *  Created on: Feb 17, 2011
 *      Author: ssy
 */

#ifndef MYMESSAGEBOX_H_
#define MYMESSAGEBOX_H_

#include <QPushButton>
#include <QWidget>
#include <QString>
#include <QLabel>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include "MyDialog.h"

enum MyMessageBoxType
{
    MB_QUESTION,
    MB_OK,
    MB_ERROR
};

class MyMessageBox: public MyDialog {
    Q_OBJECT
public:
    MyMessageBox(QWidget *parent, MyMessageBoxType type, const QString &text);
    virtual ~MyMessageBox();

    DialogCode retCode() { return mRetCode; }

private:
    QPushButton* okButton;
    QPushButton* yesButton;
    QPushButton* noButton;

    QLabel* label;
    QVBoxLayout* layout;
    QVBoxLayout* upperLayout;
    QHBoxLayout* lowerLayout;

    DialogCode mRetCode;

private slots:

    void onOkClicked();
    void onYesClicked();
    void onNoClicked();

};

#endif /* MYMESSAGEBOX_H_ */
