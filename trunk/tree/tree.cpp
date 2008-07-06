
#include <sstream>
#include <iostream>
#include <iomanip>

 using namespace std;



int main()
{
    ostringstream os;
    float f = 5.057;

    os << fixed << setw(6) << f; 
    cout << os.str() << endl;

    return  0;
}
