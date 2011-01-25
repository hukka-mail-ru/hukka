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
    Dialog(parent),
    mLanguageIndex(LANGUAGE_RUSSIAN)
{
    setWindowTitle(tr("Options"));

    QList<QString> path;
    QString serverName = XML::instance().readValue(XML_CONFIG_FILENAME, path << XML_NODE_SERVER << XML_NODE_NAME); path.clear();
    QString serverPort = XML::instance().readValue(XML_CONFIG_FILENAME, path << XML_NODE_SERVER << XML_NODE_PORT); path.clear();
    mLanguageIndex = XML::instance().readValue(XML_CONFIG_FILENAME, path << XML_NODE_CLIENT << XML_NODE_LANGUAGE); path.clear();

    serverNameLabel = new QLabel(tr("Server"), this);
    serverPortLabel = new QLabel(tr("Port"), this);
    languageLabel = new QLabel(tr("Language"), this);

    serverNameEdit = new QLineEdit(serverName, this);
    serverPortEdit = new QLineEdit(serverPort, this);

    languageComboBox = new QComboBox(this);
    languageComboBox->addItem(tr("English"));
    languageComboBox->addItem(tr("Russian"));
    languageComboBox->setCurrentIndex(mLanguageIndex.toInt());
    connect(languageComboBox, SIGNAL(activated(int)),
            this, SLOT(onLanguageComboBoxActivated(int)));

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


    this->show();
}


void OptionsDialog::onLanguageComboBoxActivated(int lang)
{
    qDebug() << "onLanguageComboBoxActivated " << lang;
    mNewLanguageIndex = QString::number(lang);
}

void OptionsDialog::onOkClicked()
{
    QList<QString> path;
    XML::instance().writeValue(XML_CONFIG_FILENAME, path << XML_NODE_SERVER << XML_NODE_NAME, serverNameEdit->text()); path.clear();
    XML::instance().writeValue(XML_CONFIG_FILENAME, path << XML_NODE_SERVER << XML_NODE_PORT, serverPortEdit->text()); path.clear();
    XML::instance().writeValue(XML_CONFIG_FILENAME, path << XML_NODE_CLIENT << XML_NODE_LANGUAGE, mNewLanguageIndex);  path.clear();

    if(mNewLanguageIndex != mLanguageIndex)
    {
        MainWindow::instance()->showMessage(
                tr("Please restart the game to apply the language settings"));
    }

    hide();
}


void OptionsDialog::onExitClicked()
{
    hide();
}

