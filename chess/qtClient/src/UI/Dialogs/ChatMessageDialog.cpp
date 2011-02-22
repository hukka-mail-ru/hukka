#include <QDebug>
#include <QKeyEvent>

#include "ChatMessageDialog.h"
#include <Defines.h>
#include <Client.h>
#include <UI.h>

ChatMessageDialog::ChatMessageDialog(ChatType chatType, QWidget *parent):
    MyDialog(parent), mChatType(chatType)
{
    setWindowTitle(tr("Send Message"));

    label = new QLabel(tr("Your chat message:"), this);
    mEdit = new QLineEdit(this);
    okButton = new QPushButton(tr("OK"), this);

  //  resize(MAIN_WINDOW_WIDTH/2, 0);

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
        TABLEID tableId = UI::instance()->getGameTable();

        if(mChatType == CT_COMMON_CHAT)
        {
            Client::instance()->sendCommonChatMessage(LOGIC_ID_CHESS, mEdit->text());
        }
        else if(mChatType == CT_TABLE_CHAT)
        {
            Client::instance()->sendTableChatMessage(LOGIC_ID_CHESS, tableId, mEdit->text());
        }
    }

    close();
}
