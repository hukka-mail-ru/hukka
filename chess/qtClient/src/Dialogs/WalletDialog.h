/*
 * WalletDialog.h
 *
 *  Created on: Oct 7, 2011
 *      Author: ssy
 */

#ifndef WALLETDIALOG_H_
#define WALLETDIALOG_H_

#include <MyDialog.h>
#include <QPushButton>
#include <QLabel>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QLineEdit>


class WalletDialog: public MyDialog
{
    Q_OBJECT;
public:
    WalletDialog(QWidget *parent = 0);
    virtual ~WalletDialog();

    QPushButton* exitButton;
    QPushButton* okButton;
    QLabel* label;
    QLineEdit* pinEdit;

    QVBoxLayout* upperLayout;
    QHBoxLayout* lowerLayout;
    QVBoxLayout* layout;

private slots:
    void onExitClicked();
    void onOkClicked();
    void onBalanceReplenished(unsigned sum);

};

#endif /* WALLETDIALOG_H_ */
