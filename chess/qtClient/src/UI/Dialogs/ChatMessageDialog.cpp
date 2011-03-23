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

    QString header = (mChatType == CT_COMMON_CHAT) ?
                     tr("A chat message to ") + mAddressee + ":" :
                     tr("A chat message:");

    label = new QLabel(header, this);
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

        QString msg;
        if (mChatType == CT_COMMON_CHAT)
        {
            msg = (mAddressee == ADDRESSEE_ALL) ? mEdit->text() : mAddressee + ", " + mEdit->text();
        }
        else
        {
            msg = mEdit->text();
        }

        Client::instance()->sendChatMessage(LOGIC_ID_CHESS, tableID, msg);
    }

    close();
}
