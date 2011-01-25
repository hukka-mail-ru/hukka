
#ifndef SendMessageDialog_H_
#define SendMessageDialog_H_

#include <QLineEdit>
#include <QHBoxLayout>
#include <QVBoxLayout>
#include <QLabel>
#include <QPushButton>
#include <QCheckBox>
#include <QGridLayout>
#include <Dialog.h>
#include <Defines.h>


class SendMessageDialog: public QDialog
{
    Q_OBJECT
public:
    SendMessageDialog(ChatType chatType, QWidget *parent = 0);

private:


    QLineEdit* mEdit;

    QPushButton* okButton;

    QHBoxLayout* layout;

    ChatType mChatType;

private slots:

    void onOkClicked();
  //  void onExitClicked();

  //  void onAuthorized();

};

#endif /* SendMessageDialog_H_ */
