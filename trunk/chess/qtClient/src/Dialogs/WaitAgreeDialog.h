#ifndef WAITAGREEDIALOG_H_
#define WAITAGREEDIALOG_H_

#include <QPushButton>
#include <QLabel>
#include <QVBoxLayout>

#include <MyDialog.h>

class WaitAgreeDialog: public MyDialog
{
Q_OBJECT
public:
    WaitAgreeDialog(QWidget *parent = 0);

private:
    QPushButton* exitButton;
    QLabel* label;
    QVBoxLayout* layout;

private slots:
    void onExitClicked();
    void onGameStarted();
    void onGameRejected();
    void onGameOver(int status, int rating);

};




#endif /* WAITAGREEDIALOG_H_ */
