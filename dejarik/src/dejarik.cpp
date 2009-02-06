#include <iostream>
using namespace std;

#include "Board.h"

int main()
{
   cout << "Hello4" << endl;
   
   try
   {
       Board board;
       
       CellPtr center = board.getCell(0,0);
    
       PiecePtr king (new Piece("King", center, 0, 0, 1));
       center->piece = king;
       
       bool res = board.isMoveValid(king, board.getCell(0,0));
       
   }
   catch(string& err)
   {
       cerr << "EXCEPTION: " << err << endl;
   }

   return 0;
}
