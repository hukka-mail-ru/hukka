
#ifndef ChatMessageDialog_H_
#define ChatMessageDialog_H_

#include <QLineEdit>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QCheckBox>
#include <QGridLayout>
#include <MyDialog.h>
#include <Defines.h>


class ChatMessageDialog: public MyDialog
{
    Q_OBJECT
public:
    ChatMessageDialog(ChatType chatType, QWidget *parent = 0);

private:

    QLabel* label;
    QLineEdit* mEdit;

    QPushButton* okButton;

    QVBoxLayout* layout;

    ChatType mChatType;

private slots:

    void onOkClicked();
  //  void onExitClicked();

  //  void onAuthorized();

};

#endif /* ChatMessageDialog_H_ */
