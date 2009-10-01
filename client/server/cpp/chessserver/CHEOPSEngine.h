#ifndef CHESSENGINE_H_
#define CHESSENGINE_H_

#include "IChessEngine.h"

#include "./engine/ChessBoard.h"


class CHEOPSEngine :public IChessEngine
{
public:
	CHEOPSEngine();
	CHEOPSEngine( String _strGameName );
	virtual ~CHEOPSEngine();
	
    /**
     * @brief Get position on board
     * @return SPosition*
     */
    virtual const TVByte* getPosition();

    /** 
     * @brief Return nesessary size of position on board
     * @return uint32_t
     */ 
    virtual uint32_t getPosSize();

    /**
     * @brief Get the result of last move
     * @return ChessGameStatus 
     */
    virtual ChessGameStatus getResult();
    
    /**
     * @brief Check the query
     * @return true if White step
     */
    virtual bool isWhiteStep();

    /**
     * @brief Move process
     * @param _strMove: String
     * Move string in format "e2e4"
     * @return true, if move is legal, false else 
     */
    virtual bool move( const String& _strMove );
	
    /**
     * @brief Set start position to board
     * @param _pPosition: SPosition*
     * return void
     */
    virtual bool setPosition( const TVByte *_pPosition );
    

private:
    
    bool parseMove( const String& _strMove, SMove *_pMove );
        
    ChessBoard      m_Board;
    String          m_strGameName;
    ChessGameStatus m_Status;
    bool            m_isWhite;
    TVByte           m_vecbtPosition;
};

#endif /*CHESSENGINE_H_*/
