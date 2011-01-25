#include <QApplication>
#include <QTranslator>
#include <QDebug>
#include <MainWindow.h>
#include <Defines.h>
#include <QLibraryInfo>
#include <QDesktopWidget>
#include <QDir>
#include <XML.h>
#include "UI.h"
#include "Exception.h"

int main(int argc, char *argv[])
{
    try
    {
        QApplication app(argc, argv);

        QString languageIndex = XML::instance().readValue(XML_CONFIG_FILENAME, QList<QString>() << XML_NODE_CLIENT << XML_NODE_LANGUAGE);

        // Translation
        QTranslator qtTranslator;
        QTranslator appTranslator;
        if(languageIndex == LANGUAGE_RUSSIAN)
        {
            qtTranslator.load("qt_ru.qm", QLibraryInfo::location(QLibraryInfo::TranslationsPath));
            app.installTranslator(&qtTranslator);

            appTranslator.load(":/translate/strings_ru.qm");
            app.installTranslator(&appTranslator);
        }

        // RUN
        UI::instance()->initialize(&app);

        int res = app.exec();

        delete UI::instance();
        delete MainWindow::instance();

        qDebug() << "main finished";

        return res;
    }
    catch (Exception& e)
    {
        qDebug() << e.what();
        throw e;
    }
}

