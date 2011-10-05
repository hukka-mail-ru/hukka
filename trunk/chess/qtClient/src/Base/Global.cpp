/*
 * Global.cpp
 *
 *  Created on: Sep 26, 2011
 *      Author: ssy
 */
#include "Global.h"
#include <QObject>

char Global::letter(CELLID cell)
{
    return 'a' + cell % CELLS_IN_ROW;
}

char Global::number(CELLID cell)
{
    return '1' + cell / CELLS_IN_ROW;
}

QString Global::seconds2hrs (unsigned seconds)
{
    unsigned hrs = seconds / SECONDS_IN_HOUR;
    unsigned mins = (seconds - hrs * SECONDS_IN_HOUR) / SECONDS_IN_MINUTE;
    unsigned secs = seconds - hrs * SECONDS_IN_HOUR - mins * SECONDS_IN_MINUTE;

    QChar fill = QLatin1Char('0');

    return QString("%1:%2:%3")
            .arg(hrs, 1, 10, fill)
            .arg(mins, 2, 10, fill)
            .arg(secs, 2, 10, fill);
}

QString Global::timestamp()
{
    return QString::number(QTime::currentTime().minute()).rightJustified(2, '0') + ":" +
           QString::number(QTime::currentTime().second()).rightJustified(2, '0') + "." +
           QString::number(QTime::currentTime().msec()).rightJustified(3, '0');
}

char Global::getCRC(const QByteArray& data)
{
    char crc = 0;
    for(int i = 0; i < data.size(); i++) {
        crc ^= data[i];
    }

    return crc;
}

QString Global::serviceToString(unsigned service)
{
  //  QT_TRACEOUT;

    switch(service) {
        case SRV: return "SRV";
        case REG: return "REG";
        case TBM: return "TBM";
        case CHS: return "CHS";
        case CHAT: return "CHAT";
        default:  return QString::number((int)service);
    }
}

QString Global::paramIdToString(int paramId)
{
    QString res;
    switch(paramId) {
        case  PARAMETER_ID_PLAYER_NAME     : res = QObject::tr("Player name"); break;
        case  PARAMETER_ID_OPPONENT_NAME   : res = QObject::tr("Opponent name");  break;
        case  PARAMETER_ID_PASSWD          : res = QObject::tr("Password"); break;
        case  PARAMETER_ID_MOVETIME        : res = QObject::tr("Move time"); break;
        case  PARAMETER_ID_GAMETIME        : res = QObject::tr("Game time"); break;
        case  PARAMETER_ID_MINRATING       : res = QObject::tr("Min. rating");  break;
        case  PARAMETER_ID_MAXRATING       : res = QObject::tr("Max. rating");  break;
        case  PARAMETER_ID_BET             : res = QObject::tr("Bet"); break;
        default : res = QString::number(paramId);
    }

    return res;
}

bool Global::isFieldEmpty(const Field& field)
{
    int allPieces = Empty;
    for(unsigned i=0; i<field.size(); i++)
    {
        allPieces += (int)field[i];
    }

    return (field.empty() || allPieces == Empty);
}


QString Global::getGameResultText(int status, int rating)
{
    QString text = "";
    QString ratingText = QString::number(rating);
    QString ratingIncreased = QObject::tr("Your rating has been increased to");
    QString ratingDecreased = QObject::tr("Your rating has been decreased to");
    QString ratingSlightlyIncreased = QObject::tr("Your rating has been slightly increased.");
    QString ratingNotAffected = QObject::tr("Your rating hasn't been affected because of too few number of moves.");
    QString ratingUnavailable = QObject::tr("Your rating is not available.\nPlease visit www.site.com to learn how to enable it.");

    switch(status)
    {
        case P_WIN:        text = QObject::tr("You have won!") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingIncreased + " " + ratingText;
                           break;

        case P_WIN_TIME:   text = QObject::tr("Your opponent's time is up. You have won!") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingIncreased + " " + ratingText;
                           break;

        case P_WIN_SURRENDER:   text = QObject::tr("Your opponent has surrendered!") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingIncreased + " " + ratingText;
                           break;

        case P_LOOSE:      text = QObject::tr("You have lost!") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingDecreased + " " + ratingText;
                           break;

        case P_LOOSE_TIME: text = QObject::tr("Time's up. You have lost!") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingDecreased + " " + ratingText;
                           break;

        case P_LOOSE_SURRENDER: text = QObject::tr("You have surrendered.") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingDecreased + " " + ratingText;
                           break;

        case P_DRAW:       text = QObject::tr("A draw.") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingSlightlyIncreased;
                           break;

        case P_DRAW_STALEMATE:       text = QObject::tr("A draw (a stalemate).") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingSlightlyIncreased;
                           break;

        case P_DRAW_TRIPPLE_OCCURRENCE:       text = QObject::tr("A draw (the same position has occurred three times).") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingSlightlyIncreased;
                           break;

        case P_DRAW_FIFTY_MOVES:       text = QObject::tr("A draw (there has been no capture or pawn move in the last 50 moves).") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingSlightlyIncreased;
                           break;

        case P_NO_RES:     text = QObject::tr("Game over.") + "\n\n";
                           text += (rating == RATING_NOT_AVAILABLE) ? ratingUnavailable :
                                   ratingNotAffected;
                           break;

        default:   break;
    }

    return text;
}
