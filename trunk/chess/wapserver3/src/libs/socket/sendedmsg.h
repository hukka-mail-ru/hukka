#ifndef CSENDEDMSG_H
#define CSENDEDMSG_H

#include "../tools/structs.h"
#include "../tools/mystr.h"

/**
 *@file sendedmsg.h
 *@author leha
 *@class CSendedMsg
 *@brief Так как меня задолбало шаманить с векторами каждый раз когда мне надо послать сообщение в сокет я решил сделать класс который будет шаманить за меня
 */

class CSendedMsg
{
public:
    CSendedMsg();
    virtual ~CSendedMsg();

    /**
     *@brief Добавить данные в отсылаемое сообщение
     *@param Метод перегружен. Идеально - для всех типов проекта.
     *@return void
     */
    void addData( char _char );
    void addData( TVecByte* _pvecByte );
    void addData( TVecUINT* _pvecUINT );
    void addData( uint32_t );
    void addData( CMyStr* _pStr );

    /**
     *@brief Получить адрес по которому располагаются данные и размер
     *@param _pData: char* указатель на начало блока данных в памяти
     *@param _nSize: uint32_t размер блока данных в памяти
     *@return void
     */
    void getData( char* _pData, uint32_t& _nSize );
    
    /**
     *@brief Метод возвращает данные в виде вектора байт
     *@return TVecByte
     */
    TVecByte getDataVec() { return m_vecData; }

private:
    TVecByte    m_vecData;

};
#endif

