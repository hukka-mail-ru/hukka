#ifndef PIXMAPS_H_
#define PIXMAPS_H_

#include <QHash>
#include <QPixmap>
#include <ChessTypes.h>

enum PixmapKey
{
    PIX_BOARD = 101,
    PIX_PIECES = 102,


    PIX_NONE = 0,
    PIX_WHITE_PAWN = w_Pawn,
    PIX_WHITE_ROOK = w_Rook,
    PIX_WHITE_KNIGHT = w_Knight,
    PIX_WHITE_BISHOP = w_Bishop,
    PIX_WHITE_QUEEN = w_Queen,
    PIX_WHITE_KING = w_King,
    PIX_BLACK_PAWN = b_Pawn,
    PIX_BLACK_ROOK = b_Rook,
    PIX_BLACK_KNIGHT = b_Knight,
    PIX_BLACK_BISHOP = b_Bishop,
    PIX_BLACK_QUEEN = b_Queen,
    PIX_BLACK_KING = b_King,

    PIX_BUTTON_CREATE_GAME = 1000,
    PIX_BUTTON_FIND_GAME = 1001,
    PIX_BUTTON_CHAT = 1002,
    PIX_BUTTON_OPTIONS = 1003,
    PIX_BUTTON_MENU = 1004,
    PIX_BUTTON_EXIT = 1005,

    PIX_SPLASH = 2000,
    PIX_BUTTONS = 2001,


    PIX_CELLS = 3000,
    PIX_CELL_WHITE = 3001,
    PIX_CELL_BLACK = 3002,
    PIX_CELL_HIGHLIGHT = 3003
};


typedef QHash<PixmapKey, QPixmap>     PixmapHash;
typedef vector<PixmapKey>             Field;

class Pixmaps
{
public:
    static void loadPixmaps();

    static QPixmap& get(PixmapKey key);

private:

    static void loadPixmap(PixmapKey key, const QString& filename);
    static PixmapHash mPixmaps;
};

#endif /* GAME_H_ */
