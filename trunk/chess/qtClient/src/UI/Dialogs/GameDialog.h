#ifndef GAMEDIALOG_H_
#define GAMEDIALOG_H_

#include <QPushButton>
#include <QVBoxLayout>
#include "Dialog.h"


class GameDialog: public Dialog
{
    Q_OBJECT
public:
    GameDialog(QWidget *parent = 0);

private:

    QPushButton* surrenderButton;
    QPushButton* drawButton;
    QPushButton* returnButton;

    QVBoxLayout* layout;

private slots:

    void onSurrenderClicked();
    void onDrawClicked();
    void onReturnClicked();
};

#endif /* GAMEDIALOG_H_ */
