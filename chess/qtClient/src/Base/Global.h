/*
 * Global.h
 *
 *  Created on: Sep 26, 2011
 *      Author: ssy
 */

#ifndef GLOBAL_H_
#define GLOBAL_H_

#include <Defines.h>

class Global
{
public:
    static char letter(CELLID cell);
    static char number(CELLID cell);

    static QString seconds2hrs (quint32 seconds);
    static QString timestamp();

    static char getCRC(const QByteArray& data);

    static QString serviceToString(quint32 service);

    static bool isFieldEmpty(const Field& field);

    static QString getGameResultText(int status, int rating);
};


#endif /* GLOBAL_H_ */
