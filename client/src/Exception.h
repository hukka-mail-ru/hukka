#ifndef EXCEPTION_H
#define EXCEPTION_H

#include <QString>

#define THROW_EXCEPTION(MESSAGE)        throw(Exception(__FILE__, __LINE__, MESSAGE));

class Exception
{
public:

    Exception(const QString msg = QString::null): mMsg(msg) 
    { 
   //     qDebug() << "EXCEPTION! " << mMsg; 
    }
    
    Exception(const char* file, int line, const QString msg = QString::null) 
    { 
     //   qDebug() << "EXCEPTION! (" << file << ":" << line << ")" << msg;
	mMsg += msg;
	mMsg += " (Exception at ";
	mMsg += file;
	mMsg += ":";
	mMsg += QString::number(line);
	mMsg +=  ")";
    }

    QString what() const { return mMsg; }

private:

    QString mMsg;

};

#endif

