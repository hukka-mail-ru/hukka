#include <QDir>
#include <QFile>
#include <QDebug>
#include <QTextStream>
#include "XML.h"
#include <Defines.h>
#include <Exception.h>


QString XML::readValue(QString filename, const QList<QString>& nodenames)
{
    QFile file(filename);

    if (!file.open(QIODevice::ReadOnly | QIODevice::Text))
    {
        //qDebug()  << "XML::readValue. Can't open file: " << XML_CONFIG_FILENAME;
        THROW_EXCEPTION("XML::readValue. Can't open file: " + XML_CONFIG_FILENAME);
        return "";
    }

    mConfig.setContent( &file );

    QDomNode node = mConfig.documentElement();
    for(int i=0; i<nodenames.size(); i++)
    {
        node = node.firstChildElement(nodenames[i]);
        if(node.isNull())
        {
            QString err = "Node ";
            for(int j=0; j<=i; j++)
            {
                err += "/";
                err += nodenames[j];
            }
            err +=  " is null";
            THROW_EXCEPTION("Can't read XML value. " + err);
        }
    }

    return node.attributes().namedItem(XML_VALUE_TAG).nodeValue();
}


void XML::writeValue(QString filename, const QList<QString>& nodenames, const QString& value)
{
    QDomNode node = mConfig.documentElement();
    for(int i=0; i<nodenames.size(); i++)
    {
        node = node.firstChildElement(nodenames[i]);
        if(node.isNull())
        {
            QString err = "Node ";
            for(int j=0; j<=i; j++)
            {
                err += "/";
                err += nodenames[j];
            }
            err +=  " is null";
            qDebug()  << err;
            THROW_EXCEPTION("Node is null");
        }
    }

    node.attributes().namedItem(XML_VALUE_TAG).setNodeValue(value);

    QFile fileOut(filename);

    if (!fileOut.open(QIODevice::WriteOnly | QIODevice::Text))
    {
        qDebug()  << "XML::writeValue. Can't open file: " << XML_CONFIG_FILENAME;
        return;
    }

    QTextStream out(&fileOut);
    out.setCodec("UTF-8");
    mConfig.save(out, XML_FILE_TAG_IDENT);

}

