#ifndef MOVELOADER_H_
#define MOVELOADER_H_

#include "WPTypes.h"

typedef std::pair<StrVector, StrVector> MovesVectors;  

/**
 * @file MoveLoader.h
 * @author leha
 * @class MoveLoader
 * @brief Class to load moves and convert them to 
 * pair of strings vactors of white and black moves
 */ 

class MoveLoader
{
public:
	MoveLoader();
	virtual ~MoveLoader();
	
	/**
	 * @breif Method to load file with moves
	 * @param _strFileName: String file name
	 * @return true if loading done, false else
	 */	
	bool readFile( const String& _strFileName );
	
	/**
	 * @brief getting pair of moves vectors
	 * @return poiner to pair of strings vectors
	 */	 
	const MovesVectors* getMoves() const;
	
private:
     
    MovesVectors    m_MovesVectors;
	
};

#endif /*MOVELOADER_H_*/
