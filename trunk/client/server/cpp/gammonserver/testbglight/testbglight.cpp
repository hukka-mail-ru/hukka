#include "../gammonserver/gammonlogic.h"
#include "testbglight.h"

#include <iostream>
#include <fstream>

using namespace std;

int main()
{
    char* szFileName = "log.txt";

    CGammonLogic GammonLogic;

    LoadPos(szFileName, GammonLogic);

    SavePos(szFileName, GammonLogic);

	ShowPos(GammonLogic);

    char cell;
    int nCount = 0; 

    do
    {
       TVByte vecByte(9);
       std::fill(vecByte.begin(), vecByte.end(), -1);

       const TVByte* vecPos = GammonLogic.GetPosForClient();
       int nSize = vecPos->at(CGammonLogic::DICE0) == vecPos->at(CGammonLogic::DICE1) ? 8 : 4;

       int nCount = 0; 

       vecByte.at(8) = vecPos->at(CGammonLogic::STEP);

       do
        {
            cout << " FROM (type '0' for end move; type '7' for clear move; type '8' for quit) : ";

            cin >> cell;

            if ( cell != '0' && cell != '7' && cell != '8' )
            { 
                vecByte.at(nCount) = cell - 'a' + 1;

                ++nCount;

                if (cell == '7') 
                    nCount = 0;

                cout << "POINT (type '0' for end move; type '7' for clear move; type '8' for quit) : ";

                cin >> cell;

                if (cell != '0')
                {
                    if ( vecPos->at(CGammonLogic::STEP) == 0 )
                         vecByte.at(nCount) = vecByte.at(nCount - 1) -  ( cell - '1' + 1);
                    else
                         vecByte.at(nCount) = vecByte.at(nCount - 1) +  ( cell - '1' + 1);
                }

                ++nCount;

                if (cell == '7') 
                    nCount = 0;
            }
      
        }
        while(nCount < nSize && cell != '0' && cell != '8');

        if (cell != '8')
        {
            for(TVByte::const_iterator it = vecByte.begin(); it != vecByte.end(); ++it)
                cout << (int) *it << " ";

            cout << endl;

            IGameLogic::StepRes res = GammonLogic.StepAnl(&vecByte);

            if ( res == IGameLogic::Valid )
            {
                cout << "valid move" << endl;

                SavePos(szFileName, GammonLogic);

                ShowRes(&vecByte);

                ShowPos(GammonLogic);
            }
            else if (res == IGameLogic::NotValid)
            {
                cout << "invalid move !!!" << endl;
            }

            GammonLogic.GetPosForDB();
        }
    }
    while(cell != '8');

    return 0;
}



void ShowPos(CGammonLogic& _GammonLogic)
{
    const TVByte* vecByte = _GammonLogic.GetPosForClient();

    cout << endl << endl << "  " ;	

    for(int i = 0; i < CGammonLogic::BAR_X/2; ++i)
    {
        cout << ' ' << (char)('a' + 12 + i) << ' '; 
    }
    cout << endl << "---------------------------------------" << endl << "| ";	

    for(int i = CGammonLogic::BAR_X/2; i < CGammonLogic::BAR_X; ++i)
    {
        if ( (int)vecByte->at(i) >= 0 )
            cout << ' ';

        cout << (int)vecByte->at(i) << ' '; 
    }

    cout << "|" << endl << endl << "| ";	

    for(int i = CGammonLogic::BAR_X/2 - 1; i >= 0; --i)
    {
        if ( (int)vecByte->at(i) >= 0 )
            cout << ' ';

        cout << (int)vecByte->at(i) << ' '; 
    }
    cout << "|" << endl << "---------------------------------------"  << endl << "  " ;		


    for(int i = 0; i < CGammonLogic::BAR_X/2; ++i)
    {
        cout << ' ' << (char)('a' + 11 - i) << ' '; 
    }

    cout << endl  << endl  << "BAR X : " << (int)vecByte->at(CGammonLogic::BAR_X) << " BAR O : " << (int)vecByte->at(CGammonLogic::BAR_O) << endl
         << "DICE : " << (int)vecByte->at(CGammonLogic::DICE0) << " : " << (int)vecByte->at(CGammonLogic::DICE1) << endl
         << "STEP : " << (vecByte->at(CGammonLogic::STEP) == 0 ? 'X' : 'O') << endl;
 
}

void LoadPos(char* szFileName, CGammonLogic& _GammonLogic)
{
    fstream stream(szFileName, ios_base::in);

    TVByte vecByte;

    char data;

    while( !stream.eof() && stream.good() )
    {
        data = stream.get();
        if (stream.good())
            vecByte.push_back(data);
    }

    if (vecByte.size())
        _GammonLogic.SetPos(vecByte);

}

void SavePos(char* szFileName, CGammonLogic& _GammonLogic)
{
    fstream filestream(szFileName, ios_base::trunc | ios_base::out);

    const TVByte* vecPos = _GammonLogic.GetPosForDB();

    for( TVByte::const_iterator it = vecPos->begin(); it != vecPos->end(); ++it)
      filestream << *it;

}

void ShowRes(const TVByte* _data)
{
    cout  << "DICE : " << (int)_data->at(0) << " : " << (int)_data->at(1)  << endl
          << "STEP : " << (_data->at(2) == 0 ? 'X' : 'O') << endl;
}