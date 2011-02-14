#ifndef WaitJoinDialog_H_
#define WaitJoinDialog_H_

#include <QDialog>
#include <QPushButton>
#include <QLabel>
#include <QVBoxLayout>

class WaitJoinDialog: public QDialog
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
    void onOpponentJoined();
    void onGameStarted();
    void onGameTableDeleted();
    void onError(const QString& what);
};

#endif /* WaitJoinDialog_H_ */
