#ifndef BOARD_H_
#define BOARD_H_

#include <vector>
#include "Cell.h"

class Board // only one board in game
{
private:
    vector<CellPtr> cells;
};

#endif /*BOARD_H_*/
