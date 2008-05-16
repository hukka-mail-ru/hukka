#define QT3_SUPPORT 1

#include <Qt/qapplication.h>
#include <Qt/qlabel.h>



int main(int argc, char *argv[])
{
  QApplication MyApp(argc, argv);
  QLabel Label(NULL);
  Label.setFrameStyle(QFrame::Panel | QFrame::Sunken);
  Label.setText("Hello, brave new Qt world!");
  MyApp.setMainWidget(&Label);
  Label.show();
  return MyApp.exec();
 }
