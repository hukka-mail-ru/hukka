#include <iostream>
#include <algorithm>
#include <vector>
#include <iterator>
using namespace std;


// an unary function
void Print(int i)
{
    cout << "Fuck " << i << endl;
}

// an unary function
int Square(int i)
{
    return i*i;
}

// a binary function
int Add(int i, int j)
{
    return i + j;
}

int main()
{
    vector<int> v;

    v.push_back(1);
    v.push_back(2);
    v.push_back(3);

    for_each(v.begin(), v.end(), Print);


    vector<int> res(v.size()); // size fo "res" must be >= size of "v"

	cout << "Squared:" << endl;
    transform(v.begin(), v.end(), res.begin(), Square); // transform with unary function
    for_each(res.begin(), res.end(), Print);

    vector<int> u;
    u.push_back(1);
    u.push_back(2);
    u.push_back(3);

    transform(v.begin(), v.end(), u.begin(), res.begin(), Add); // transform with binary functi
    cout << "Added:" << endl;
    for_each(res.begin(), res.end(), Print);
}
