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

    QString serverName = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_SERVER << XML_NODE_NAME);
    QString serverPort = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_SERVER << XML_NODE_PORT);
    int languageIndex = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_CLIENT << XML_NODE_LANGUAGE).toInt();

    serverNameLabel = new QLabel(tr("Server"), this);
    serverPortLabel = new QLabel(tr("Port"), this);
    languageLabel = new QLabel(tr("Language"), this);

    serverNameEdit = new QLineEdit(serverName, this);
    serverPortEdit = new QLineEdit(serverPort, this);

    languageComboBox = new QComboBox(this);
    languageComboBox->addItem(tr("English"));
    languageComboBox->addItem(tr("Russian"));
    languageComboBox->setCurrentIndex(languageIndex);

    edits = new QGridLayout();
    edits->addWidget(serverNameLabel, 0, 0);
    edits->addWidget(serverPortLabel,   2, 0);
    edits->addWidget(languageLabel,   4, 0);

    edits->addWidget(serverNameEdit,  0, 2);
    edits->addWidget(serverPortEdit,    2, 2);
    edits->addWidget(languageComboBox,    4, 2);

    upperLayout = new QVBoxLayout();
    upperLayout->addLayout(edits);
    upperLayout->addStretch();

    okButton = new QPushButton(tr("OK"), this);
    exitButton = new QPushButton(tr("Exit"), this);

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
    XML::instance().writeValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_SERVER << XML_NODE_NAME, serverNameEdit->text());
    XML::instance().writeValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_SERVER << XML_NODE_PORT, serverPortEdit->text());
    XML::instance().writeValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_CLIENT << XML_NODE_LANGUAGE, QString::number(languageComboBox->currentIndex()));

    MainWindow::instance()->showMessage(
             tr("Please restart the game to apply the settings"));

    close();
}


void OptionsDialog::onExitClicked()
{
    close();
}

