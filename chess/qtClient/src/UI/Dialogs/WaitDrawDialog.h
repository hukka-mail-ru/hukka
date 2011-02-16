#ifndef WAITDRAWDIALOG_H_
#define WAITDRAWDIALOG_H_

#include <MyDialog.h>
#include <QPushButton>
#include <QLabel>
#include <QVBoxLayout>

class WaitDrawDialog: public MyDialog
{
Q_OBJECT
public:
    WaitDrawDialog(QWidget *parent = 0);

private:
    QPushButton* exitButton;
    QLabel* label;
    QVBoxLayout* layout;

private slots:
    void onExitClicked();
    void onDrawRejected(const QString&);

};



#endif /* WAITDRAWDIALOG_H_ */
