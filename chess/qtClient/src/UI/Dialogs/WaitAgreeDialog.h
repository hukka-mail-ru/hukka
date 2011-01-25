#ifndef WAITAGREEDIALOG_H_
#define WAITAGREEDIALOG_H_

#include <QDialog>
#include <QPushButton>
#include <QLabel>
#include <QVBoxLayout>

class WaitAgreeDialog: public QDialog
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
};




#endif /* WAITAGREEDIALOG_H_ */
