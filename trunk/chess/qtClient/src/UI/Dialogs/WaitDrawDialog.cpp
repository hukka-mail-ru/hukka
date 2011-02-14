#include "WaitDrawDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"

WaitDrawDialog::WaitDrawDialog(QWidget *parent):  QDialog(parent)
{
    qDebug() << "WaitDrawDialog::WaitDrawDialog";

    label = new QLabel(tr("Wait for oppponent response..."), this);
    exitButton = new QPushButton(tr("Exit"), this);
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));

    layout = new QVBoxLayout(this);
    layout->addWidget(label);
    layout->addWidget(exitButton);
    this->setLayout(layout);

    qDebug() << "WaitDrawDialog::WaitDrawDialog connect";
    connect(Client::instance(), SIGNAL(drawRejected(const QString&)), this, SLOT(onDrawRejected(const QString&)));
    connect(Client::instance(), SIGNAL(error(const QString&)), this, SLOT(onError(const QString&)));

    qDebug() << "WaitDrawDialog::WaitDrawDialog show ";
    this->show();

    qDebug() << "WaitDrawDialog::WaitDrawDialog end";
}

void WaitDrawDialog::onError(const QString& what)
{
    qDebug() << "WaitDrawDialog::onError";
    close();
}

void WaitDrawDialog::onExitClicked()
{
    if((MainWindow::instance()->showQuestion(tr("Exiting now means you surrender. Are you sure?"))))
    {
        MainWindow::instance()->setMode(MW_WAIT);
        Client::instance()->surrender(UI::instance()->getGameTable());
    }
}


void WaitDrawDialog::onDrawRejected(const QString& message)
{
    MainWindow::instance()->showMessage(message);
    close();
}
