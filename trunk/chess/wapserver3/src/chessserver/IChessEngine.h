#ifndef ICHESSENGINE_H_
#define ICHESSENGINE_H_

#include "ChessGameStatus.h"
#include "WPTypes.h"

/**
* @file IChessEngine.h
* @author leha
* @class IChessEngine
* @brief Interface for chess engine
*/


class IChessEngine
{
public:
    
    /**
     * @brief Get position on board
     * @return Bytes*
     */
    virtual const TVByte* getPosition() = 0;
    
    /** 
     * @brief Return nesessary size of position on board
     * @return uint32_t
     */ 
    virtual uint32_t getPosSize() = 0;

    /**
     * @brief Get the result of last move
     * @return  
     */
    virtual ChessGameStatus getResult() = 0;
    
    /**
     * @brief Check the query
     * @return true if White step
     */
    virtual bool isWhiteStep() = 0;

    /**
     * @brief Move process
     * @param _strMove: String
     * Move string in format "e2e4"
     * @return true, if move is legal, false else 
     */
    virtual bool move( const String& _strMove ) = 0;
    
    /**
     * @brief Set start position to board
     * @param _pPosition: SPosition*
     * return void
     */
    virtual bool setPosition( const TVByte *_pvecbtPosition ) = 0;
    

};

#endif /*ICHESSENGINE_H_*/
