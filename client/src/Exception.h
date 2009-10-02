#ifndef EXCEPTION_H
#define EXCEPTION_H

#include <QString>



class Exception
{
public:

    Exception(const QString msg = QString::null): mMsg(msg) { qDebug() << "EXCEPTION! " << mMsg; }
    
    Exception(const char* file, int line, const QString msg = QString::null): mMsg(msg) 
    { 
        QString linestr;
        linestr.setNum(line);
        QString str("EXCEPTION! (");
        str += file;
        str += ": ";
        str += linestr;
        str += ") ";
        str += mMsg;
        qDebug() << str; 
    }

    QString what() const { return mMsg; }

private:

    QString mMsg;

};

#endif

