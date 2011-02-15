#include "SendMessageDialog.h"
#include <Defines.h>
#include <Client.h>
#include <UI.h>

SendMessageDialog::SendMessageDialog(ChatType chatType, QWidget *parent):
    QDialog(parent), mChatType(chatType)
{
    setWindowTitle(tr("Send Message"));

    mEdit = new QLineEdit(this);
    okButton = new QPushButton(tr("OK"), this);

  //  resize(MAIN_WINDOW_WIDTH/2, 0);

    layout = new QHBoxLayout(this);
    layout->addWidget(mEdit);
    layout->addWidget(okButton);

    this->setLayout(layout);

    connect(okButton, SIGNAL(clicked()), this, SLOT(onOkClicked()));
}




void SendMessageDialog::onOkClicked()
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

    this->hide();
}
