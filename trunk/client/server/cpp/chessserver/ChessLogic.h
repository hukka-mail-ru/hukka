#ifndef CHESSLOGIC_H_
#define CHESSLOGIC_H_

#include <IChessLogic.h>
#include <CHEOPSEngine.h>

class ChessLogic : public IChessLogic
{
public:
	ChessLogic();
	virtual ~ChessLogic();
	
    
    /**
     * @brief Actual position on game table for sending to client
     * @return Butes* pointer to vector of bytes
     */
    virtual const TVByte* GetPosForClient();
	
    /**
     * @brief Actual position on game table for storing in database
     * @return Butes* pointer to vector of bytes
     */
    virtual const TVByte* GetPosForDB();

    /**
     * @brief Return status of current game
     * @return ChessGameStatus
     */
    virtual ChessGameStatus GetStatus();

    /**
     * @brief Set the initial position to the game table
     * @param _vecBytes: Bytes
     * Initial position: vector of bytes
     * @return true, if setting was done, false else 
     */
    virtual bool SetPos( const TVByte &_vecBytes );
    
    /**
     * @brief Actual player move
     * @param _vecBytes: Bytes
     * Move data
     * @return IGameLogic::StepRes: result of actual move
     */
    virtual IGameLogic::StepRes StepAnl( TVByte *_vecBytes);
    
    
private:
    CHEOPSEngine    m_Engine;
    TVByte           m_vecbtPosForClient;
};

#endif /*CHESSLOGIC_H_*/
