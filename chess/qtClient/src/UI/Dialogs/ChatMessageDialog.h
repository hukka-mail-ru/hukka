
#ifndef ChatMessageDialog_H_
#define ChatMessageDialog_H_

#include <QLineEdit>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QCheckBox>
#include <QGridLayout>
#include <QString>

#include <MyDialog.h>
#include <Defines.h>


class ChatMessageDialog: public MyDialog
{
    Q_OBJECT
public:
    ChatMessageDialog(const QString& addressee, ChatType chatType, QWidget *parent = 0);

protected:
    void keyPressEvent(QKeyEvent *);

private:


    QLabel* label;
    QLineEdit* mEdit;

    QPushButton* okButton;

    QVBoxLayout* layout;

    ChatType mChatType;

    QString mAddressee;

private slots:

    void onOkClicked();
  //  void onExitClicked();

  //  void onAuthorized();

};

#endif /* ChatMessageDialog_H_ */
