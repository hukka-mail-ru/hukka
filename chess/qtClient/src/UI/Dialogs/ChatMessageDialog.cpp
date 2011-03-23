#include <QDebug>
#include <QKeyEvent>

#include "ChatMessageDialog.h"
#include <Defines.h>
#include <Client.h>
#include <UI.h>
#include <chatserver/chatdefs.h>

ChatMessageDialog::ChatMessageDialog(const QString& addressee, ChatType chatType, QWidget *parent):
    MyDialog(parent), mChatType(chatType), mAddressee(addressee)
{
    setWindowTitle(tr("Send Message"));

    label = new QLabel(tr("A chat message to ") + mAddressee + ":", this);
    mEdit = new QLineEdit(this);
    okButton = new QPushButton(tr("OK"), this);

    layout = new QVBoxLayout(this);

    layout->addWidget(label);
    layout->addWidget(mEdit);
    layout->addStretch();
    layout->addWidget(okButton);

    this->setLayout(layout);

    mEdit->setFocus();

    connect(okButton, SIGNAL(clicked()), this, SLOT(onOkClicked()));
}

void ChatMessageDialog::keyPressEvent(QKeyEvent* e)
{
    if(e->key() == Qt::Key_Return)
    {
        onOkClicked();
    }
}

void ChatMessageDialog::onOkClicked()
{
    if(mEdit->text() != "")
    {
        TABLEID tableID = (mChatType == CT_COMMON_CHAT) ?
                          COMMON_CHAT_ID : UI::instance()->getGameTable();

        QString msg = (mAddressee == ADDRESSEE_ALL) ? mEdit->text() : mAddressee + ", " + mEdit->text();
        Client::instance()->sendChatMessage(LOGIC_ID_CHESS, tableID, msg);
    }

    close();
}
