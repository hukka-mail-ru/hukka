/*
 * Dialog.h
 *
 *  Created on: Mar 25, 2010
 *      Author: ssy
 */

#ifndef DIALOG_H_
#define DIALOG_H_

#include <QDialog>
#include <QString>
#define DEFAULT_TEXT "DEFAULT"

class Dialog: public QDialog
{
Q_OBJECT
public:
    Dialog(QWidget *parent = 0, Qt::WindowFlags flags = 0);
    virtual ~Dialog();

};

#endif /* DIALOG_H_ */
