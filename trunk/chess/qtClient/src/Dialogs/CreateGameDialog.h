#ifndef CREATEGAMEDIALOG_H_
#define CREATEGAMEDIALOG_H_

#include <QLineEdit>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QGridLayout>
#include "MyDialog.h"
#include "Defines.h"


class CreateGameDialog: public MyDialog
{
    Q_OBJECT
public:
    CreateGameDialog(QWidget *parent = 0);

private:

    QLabel* nameLabel;


    QLabel* betLabel;
    QLabel* moveTimeLabel;
    QLabel* gameTimeLabel;
    QLabel* maxRatingLabel;
    QLabel* minRatingLabel;

    QLineEdit* betEdit;
    QLineEdit* moveTimeEdit;
    QLineEdit* gameTimeEdit;
    QLineEdit* maxRatingEdit;
    QLineEdit* minRatingEdit;



    QPushButton* okButton;
    QPushButton* exitButton;

    QGridLayout* edits;

    QVBoxLayout* upperLayout;
    QHBoxLayout* lowerLayout;
    QVBoxLayout* layout;

    GameTable mGameTable; // the created game

private slots:

    void onOkClicked();
    void onExitClicked();

    void onGameTableCreated(TABLEID id);

};

#endif /* CREATEGAMEDIALOG_H_ */
