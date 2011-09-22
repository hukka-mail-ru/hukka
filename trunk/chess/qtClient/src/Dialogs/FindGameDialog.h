#ifndef FINDGAMEDIALOG_H_
#define FINDGAMEDIALOG_H_

#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QGridLayout>
#include "MyDialog.h"
#include "Defines.h"

class FindGameDialog: public MyDialog
{
Q_OBJECT
public:
    FindGameDialog(QWidget *parent = 0);

private:


    QPushButton* okButton;
    QPushButton* randomGameButton;
    QPushButton* cancelButton;

    QGridLayout* edits;

    QVBoxLayout* upperLayout;
    QHBoxLayout* lowerLayout;
    QVBoxLayout* layout;

    QList<GameTable> mTables;

private slots:

    //void onCancelClicked();
    //void onRandomGameClicked();

    void onGotGameTables(const QList<GameTable>& tables);
    void onGotGameTableParams(const GameTable& table);
};

#endif /* FINDGAMEDIALOG_H_ */

