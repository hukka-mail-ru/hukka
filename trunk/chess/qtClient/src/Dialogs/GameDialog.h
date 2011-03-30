#ifndef GAMEDIALOG_H_
#define GAMEDIALOG_H_

#include <QPushButton>
#include <QVBoxLayout>
#include "MyDialog.h"


class GameDialog: public MyDialog
{
    Q_OBJECT
public:
    GameDialog(QWidget *parent = 0);

private:

    QPushButton* surrenderButton;
    QPushButton* drawButton;
    QPushButton* returnButton;
    QPushButton* returnToMenuButton;

    QVBoxLayout* layout;

private slots:

    void onSurrenderClicked();
    void onDrawClicked();
    void onReturnClicked();
    void onReturnToMenuClicked();
};

#endif /* GAMEDIALOG_H_ */
