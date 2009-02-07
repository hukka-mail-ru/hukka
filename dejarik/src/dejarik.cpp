#include <iostream>
using namespace std;

#include "Board.h"

int main()
{
   cout << "Hello4" << endl;
   
   try
   {
       Board board;
    
       PiecePtr king (new Piece("King", 0, 0, 1));
       
       vector<CellPtr> moves;
       board.getPossibleMoves(king, moves);
       
   }
   catch(string& err)
   {
       cerr << "EXCEPTION: " << err << endl;
   }

   return 0;
}
