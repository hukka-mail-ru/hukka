/* 
 * File:   buffer.h
 * Author: leha
 *
 * Created on 7 Май 2008 г., 14:37
 */

#ifndef _BUFFER_H
#define	_BUFFER_H

class CBuffer 
{
public:
    CBuffer();
    virtual ~CBuffer();

    char*           GetDataStart();
    char*           GetDataEnd();

    int             DataSize() const;
    int             FreeSize() const;

    void            IncBuffer();
    void            AddDataSize( int _nSize );

    void            RemoveData( char* );

private:

    char*	m_pBuff;
    int		m_nBuffSize;
    int		m_nDataSize;

};


#endif	/* _BUFFER_H */

