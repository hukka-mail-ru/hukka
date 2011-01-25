#ifndef AUTHORIZATIONDIALOGS_H_
#define AUTHORIZATIONDIALOGS_H_

#include <QLineEdit>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QCheckBox>
#include <QGridLayout>
#include "Dialog.h"


class AuthorizationDialog: public Dialog
{
    Q_OBJECT
public:
    AuthorizationDialog(QWidget *parent = 0);

private:

    QLabel* loginLabel;
    QLabel* pwdLabel;

    QLineEdit* loginEdit;
    QLineEdit* pwdEdit;

    QPushButton* okButton;
    QPushButton* exitButton;

    QCheckBox* registerCheckBox;

    QGridLayout* edits;
    QHBoxLayout* box;
    QVBoxLayout* upperLayout;
    QHBoxLayout* lowerLayout;
    QVBoxLayout* layout;

private slots:

    void onOkClicked();
    void onExitClicked();

};

#endif /* DIALOGS_H_ */
