#ifndef JOINGAMEDIALOG_H_
#define JOINGAMEDIALOG_H_

#include <QLabel>
#include <QLineEdit>
#include <QPushButton>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QTableWidget>
#include <QList>
#include "Dialog.h"
#include "Defines.h"

class JoinGameDialog: public Dialog
{
Q_OBJECT
public:
    JoinGameDialog(const QList<TABLEID>& tableIDs, QWidget *parent = 0);

private:

    QPushButton* okButton;
    QPushButton* cancelButton;

    QTableWidget* tableWidget;

    QHBoxLayout* upperLayout;
    QHBoxLayout* lowerLayout;
    QVBoxLayout* layout;

    QList<TABLEID> mGameTableIDs;

    quint32 mCounter;

    void getParams(TABLEID tableID);

private slots:

    void onOkClicked();
    void onCancelClicked();
    void onJoined(TABLEID);

    void onGotGameTableParams(const QString& name, qint32 rating, qint32 time2step, qint32 time2game);

};

#endif /* JOINGAMEDIALOG_H_ */
