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

                quint32 params[] = { 4, 
                             77, 77, 
                             77, 77, 
                             77, 77};
//	h.cp = "test fgdf";

        qDebug() << sizeof(params);

        return 0;
}



