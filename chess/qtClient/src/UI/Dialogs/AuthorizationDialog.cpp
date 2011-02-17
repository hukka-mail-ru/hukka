#include <QDir>

#include "AuthorizationDialog.h"
#include "MainWindow.h"
#include "Client.h"
#include "UI.h"
#include "XML.h"

#include <QVBoxLayout>

AuthorizationDialog::AuthorizationDialog(QWidget *parent): MyDialog(parent)
{
    setWindowTitle(tr("Authorization"));

    QString login = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_USER << XML_NODE_LOGIN);
    QString pwd = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_USER << XML_NODE_PASSWORD);

    loginLabel = new QLabel(tr("Login"), this);
    pwdLabel = new QLabel(tr("Password"), this);

//////
 //   QString colour = "white";
  //  QString text = tr("Login");
 //   QString fonttemplate = tr("<font color='%1'>%2</font>");
 //   loginLabel->setText(fonttemplate.arg( colour, text ));
//////

  //  setStyleSheet("QLabel { color: red;}");

    loginEdit = new QLineEdit(login, this);
    pwdEdit = new QLineEdit(pwd, this);
    pwdEdit->setEchoMode(QLineEdit::Password);

//////
 //   QPalette palette( loginEdit->palette() );
//    palette.setColor( QPalette::Base, QColor(Qt::black) );
//    palette.setColor( QPalette::Text, QColor(Qt::white) );
//    loginEdit->setPalette(palette);


//////

    registerCheckBox = new QCheckBox(tr("Register new user"), this);
    registerCheckBox->setCheckState(Qt::Unchecked);

    okButton = new QPushButton(tr("OK"), this);
    exitButton = new QPushButton(tr("Exit"), this);

    edits = new QGridLayout();
    edits->addWidget(loginLabel, 0, 0);
    edits->addWidget(pwdLabel,   2, 0);
    edits->addWidget(loginEdit,  0, 2);
    edits->addWidget(pwdEdit,    2, 2);

    box = new QHBoxLayout();
    box->addWidget(registerCheckBox);

    upperLayout = new QVBoxLayout();
    upperLayout->addLayout(edits);
    upperLayout->addStretch();

    lowerLayout = new QHBoxLayout();
    lowerLayout->addWidget(okButton);
    lowerLayout->addWidget(exitButton);

    layout = new QVBoxLayout(this);
    layout->addLayout(upperLayout);
    layout->addLayout(box);
    layout->addLayout(lowerLayout);


    this->setLayout(layout);

    connect(okButton, SIGNAL(clicked()), this, SLOT(onOkClicked()));
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));
}

void AuthorizationDialog::onOkClicked()
{
    MainWindow::instance()->setMode(MW_WAIT);

    QList<QString> path;
    XML::instance().writeValue(XML_CONFIG_FILENAME, path << XML_NODE_USER << XML_NODE_LOGIN, loginEdit->text()); path.clear();
    XML::instance().writeValue(XML_CONFIG_FILENAME, path << XML_NODE_USER << XML_NODE_PASSWORD, pwdEdit->text()); path.clear();

    if(registerCheckBox->checkState() == Qt::Unchecked)
    {
        Client::instance()->login(loginEdit->text(), pwdEdit->text());
    }
    else
    {
        Client::instance()->registerUser(loginEdit->text(), pwdEdit->text());
    }
}


void AuthorizationDialog::onExitClicked()
{
  //  qDebug() << "AuthorizationDialog::onExitClicked()";
  //  mUI->getClient()->disconnectFromHost();
    hide();
}




