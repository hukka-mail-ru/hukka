#ifndef WAITDRAWDIALOG_H_
#define WAITDRAWDIALOG_H_

#include <QDialog>
#include <QPushButton>
#include <QLabel>
#include <QVBoxLayout>

class WaitDrawDialog: public QDialog
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
