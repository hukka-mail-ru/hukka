#ifndef EXCEPTION_H
#define EXCEPTION_H

#include <QString>



class Exception
{
public:

    Exception(const QString msg = QString::null): mMsg(msg) 
    { 
        qDebug() << "EXCEPTION! " << mMsg; 
    }
    
    Exception(const char* file, int line, const QString msg = QString::null): mMsg(msg) 
    { 
        qDebug() << "EXCEPTION! (" << file << ":" << line << ")" << msg;
    }

    QString what() const { return mMsg; }

private:

    QString mMsg;

};

#endif

