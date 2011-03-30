#ifndef WaitJoinDialog_H_
#define WaitJoinDialog_H_

#include <MyDialog.h>
#include <QPushButton>
#include <QLabel>
#include <QVBoxLayout>

#include "Defines.h"

class WaitJoinDialog: public MyDialog
{
Q_OBJECT
public:
    WaitJoinDialog(QWidget *parent = 0);

private:
    QPushButton* exitButton;
    QLabel* label;
    QVBoxLayout* layout;

private slots:
    void onExitClicked();
    void onOpponentJoined(const QString& opponentName, int opponentRating);
    void onGameStarted();
    void onGameRejected();
    void onGameTableDeleted();
};

#endif /* WaitJoinDialog_H_ */
