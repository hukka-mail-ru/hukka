#include <QObject>
#include <QDebug>

int main()
{
        const char* file = "FILE";
        int line = 10;
        QString str;
        QString msg = "MESSAGE";

        qDebug() << "EXCEPTION! (" << file << ":" << line << ")" << msg;

        return 0;
}



