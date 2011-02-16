/*
 * OptionsDialog.h
 *
 *  Created on: Jul 9, 2010
 *      Author: ssy
 */

#ifndef OPTIONSDIALOG_H_
#define OPTIONSDIALOG_H_

#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QComboBox>
#include <QGridLayout>

#include <MyDialog.h>


class OptionsDialog: public MyDialog
{
    Q_OBJECT
public:
    OptionsDialog(QWidget *parent = 0);
private:

    QLabel* serverNameLabel;
    QLabel* serverPortLabel;
    QLabel* languageLabel;

    QLineEdit* serverNameEdit;
    QLineEdit* serverPortEdit;

    QComboBox* languageComboBox;
    QString mLanguageIndex;
    QString mNewLanguageIndex;

    QPushButton* okButton;
    QPushButton* exitButton;

    QGridLayout* edits;

    QHBoxLayout* box;
    QVBoxLayout* upperLayout;
    QHBoxLayout* lowerLayout;
    QVBoxLayout* layout;

private slots:

    void onOkClicked();
    void onExitClicked();
    void onLanguageComboBoxActivated(int lang);
};

#endif /* OPTIONSDIALOG_H_ */
