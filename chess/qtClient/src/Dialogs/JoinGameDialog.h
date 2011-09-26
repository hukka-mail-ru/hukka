#ifndef JOINGAMEDIALOG_H_
#define JOINGAMEDIALOG_H_

#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QTableWidget>
#include <QList>
#include "MyDialog.h"
#include "Defines.h"

class JoinGameDialog: public MyDialog
{
Q_OBJECT
public:
    JoinGameDialog(const QList<GameTable>& tables, QWidget *parent = 0);

private:

    QPushButton* okButton;
    QPushButton* cancelButton;

    QTableWidget* tableWidget;

    QHBoxLayout* upperLayout;
    QHBoxLayout* lowerLayout;
    QVBoxLayout* layout;

    QList<GameTable> mGameTables;

    int mCurrentTable;

    void getParams(const GameTable& table);

private slots:

    void onOkClicked();
    void onCancelClicked();
    void onJoined(TABLEID);

    void onGotGameTableParams(const GameTable& table);
    void onCellPressed(int row, int column);
    void onCellClicked(int row, int column);
};

#endif /* JOINGAMEDIALOG_H_ */
