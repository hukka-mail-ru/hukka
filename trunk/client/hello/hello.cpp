#include <QObject>
#include <QDebug>

#pragma pack(1)
struct Hell
{
	char c;
	char cp[];
};

int main()
{
	Hell h;


	h.cp = "test fgdf";

        qDebug() << sizeof(h);

        return 0;
}



