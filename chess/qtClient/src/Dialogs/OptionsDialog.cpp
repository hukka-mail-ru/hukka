/*
 * OptionsDialog.cpp
 *
 *  Created on: Jul 9, 2010
 *      Author: ssy
 */

#include <QDebug>
#include <QDir>
#include <XML.h>
#include "OptionsDialog.h"
#include <Defines.h>
#include <MainWindow.h>


OptionsDialog::OptionsDialog(QWidget *parent):
    MyDialog(parent)
{
    setWindowTitle(tr("Options"));

    mLogin      = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_USER << XML_NODE_LOGIN);
    mPwd        = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_USER << XML_NODE_PASSWORD);
    QString serverName = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_SERVER << XML_NODE_NAME);
    QString serverPort = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_SERVER << XML_NODE_PORT);
    mLanguageIndex     = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_CLIENT << XML_NODE_LANGUAGE).toInt();

    loginLabel      = new QLabel(tr("Login"),    this);
    pwdLabel        = new QLabel(tr("Password"), this);
    serverNameLabel = new QLabel(tr("Server"),   this);
    serverPortLabel = new QLabel(tr("Port"),     this);
    languageLabel   = new QLabel(tr("Language"), this);

    loginEdit      = new QLineEdit(mLogin,     this);
    serverNameEdit = new QLineEdit(serverName, this);
    serverPortEdit = new QLineEdit(serverPort, this);
    pwdEdit        = new QLineEdit(mPwd,       this);
    pwdEdit->setEchoMode(QLineEdit::Password);

    languageComboBox = new QComboBox(this);
    languageComboBox->addItem(tr("English"));
    languageComboBox->addItem(tr("Russian"));
    languageComboBox->setCurrentIndex(mLanguageIndex);

    edits = new QGridLayout();
    edits->addWidget(loginLabel,      0, 0);
    edits->addWidget(pwdLabel,        2, 0);
    edits->addWidget(serverNameLabel, 4, 0);
    edits->addWidget(serverPortLabel, 6, 0);
    edits->addWidget(languageLabel,   8, 0);

    edits->addWidget(loginEdit,       0, 2);
    edits->addWidget(pwdEdit,         2, 2);
    edits->addWidget(serverNameEdit,  4, 2);
    edits->addWidget(serverPortEdit,  6, 2);
    edits->addWidget(languageComboBox,8, 2);

    upperLayout = new QVBoxLayout();
    upperLayout->addLayout(edits);
    upperLayout->addStretch();

    okButton = new QPushButton(tr("OK"), this);
    exitButton = new QPushButton(tr("Cancel"), this);

    lowerLayout = new QHBoxLayout();
    lowerLayout->addWidget(okButton);
    lowerLayout->addWidget(exitButton);

    layout = new QVBoxLayout(this);
    layout->addLayout(upperLayout);
    layout->addLayout(lowerLayout);

    this->setLayout(layout);

    connect(okButton, SIGNAL(clicked()), this, SLOT(onOkClicked()));
    connect(exitButton, SIGNAL(clicked()), this, SLOT(onExitClicked()));
}



void OptionsDialog::onOkClicked()
{
    XML::instance().writeValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_USER << XML_NODE_LOGIN, loginEdit->text());
    XML::instance().writeValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_USER << XML_NODE_PASSWORD, pwdEdit->text());
    XML::instance().writeValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_SERVER << XML_NODE_NAME, serverNameEdit->text());
    XML::instance().writeValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_SERVER << XML_NODE_PORT, serverPortEdit->text());
    XML::instance().writeValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_CLIENT << XML_NODE_LANGUAGE, QString::number(languageComboBox->currentIndex()));

    if(mLanguageIndex != languageComboBox->currentIndex() ||
       mLogin != loginEdit->text() ||
       mPwd   != pwdEdit->text())
    {
        MainWindow::instance()->showMessage(
             tr("Please restart the game to apply the settings"));
    }

    close();
}


void OptionsDialog::onExitClicked()
{
    close();
}

