#include "WaitDrawDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"

WaitDrawDialog::WaitDrawDialog(QWidget *parent):  QDialog(parent)
{
    label = new QLabel(tr("Wait for oppponent response..."), this);
    exitButton = new QPushButton(tr("Exit"), this);
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));

    layout = new QVBoxLayout(this);
    layout->addWidget(label);
    layout->addWidget(exitButton);
    this->setLayout(layout);

    connect(Client::instance(), SIGNAL(drawRejected(const QString&)), this, SLOT(onDrawRejected(const QString&)));

    this->show();
}


void WaitDrawDialog::onExitClicked()
{
    if((MainWindow::instance()->showQuestion(tr("Exiting now means you surrender. Are you sure?"))))
    {
        disconnect(Client::instance(), SIGNAL(drawRejected(const QString&)), this, SLOT(onDrawRejected(const QString&)));

        MainWindow::instance()->setMode(MW_WAIT);
        Client::instance()->surrender(UI::instance()->getGameTable());
    }
}


void WaitDrawDialog::onDrawRejected(const QString& message)
{
    disconnect(Client::instance(), SIGNAL(drawRejected(const QString&)), this, SLOT(onDrawRejected(const QString&)));

    MainWindow::instance()->showMessage(message);
    close();
}
