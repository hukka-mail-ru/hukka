#if !defined(_TBMCOMMANDS_H_)
#define _TBMCOMMANDS_H_

#include "sqllogiclist.h"

struct SFindParameters
{
    int m_nParameter;
    int m_nValue;
    uint8_t m_nCondition;
    uint8_t m_nLogic;
};

typedef std::pair<int, int> TTbmPrm; // TTbmPrm::first
typedef std::vector<TTbmPrm> TVecPrms;
typedef std::vector<SFindParameters> TVecFindPrms;

/**
 * @file tmbcommands.h
 * @author boon
 * @class CTbCommands
 * @brief Main class of tablemanager
 */

class CTbmCommands  
{

public:

	CTbmCommands();

	virtual ~CTbmCommands();

	enum CrRes { DONE, TABEX, NVPAR };
	
	/**
	 * @brief Method for creation new game table
	 * @param _nLogicID: uint32_t ID of game logic
	 * @param _nPlayerID: uint32_t ID of player who create game table
	 * @param _vecPrms: const TVecPrms parameters of new game table
	 * @param _strPswd: CMyStr password
	 * @return DONE if game table created succesfully, 
	 * TABEX if player with such PlayerID already have created game table,
	 * NVPAR if parameters for new table is not valid.
	 */

	CrRes Create(uint32_t _nLogicID, uint32_t _nPlayerID, const TVecPrms &_vecPrms, const CMyStr* _strPswd = NULL );
    
    bool Delete(uint32_t _nLogicID, uint32_t _nPlayerID, uint32_t _nTableID);
	
	/**
	 * @brief Method for searching game tables. 
	 * @param _nLogicID: uint32_t ID of game logic
	 * @param _nPlayerID: uint32_t ID of player who search game table
	 * @param _nCount: uint32_t count tables with requested parameters
	 * @param _pvecPrms: TVecPrms* requested parameters for tables
	 * @param _pvecRes: TVecUINT game tables
	 * @return FALSE if game tables wasn't founded, TRUE else
	 */
	
	bool Find(uint32_t _nLogicID, uint32_t _nPlayerID, uint32_t _nCount, const TVecPrms* _pvecPrms = NULL, TVecUINT* _pvecRes = NULL);
	
	/**
	 * @brief Method for searching game tables. 
	 * @param _nLogicID: uint32_t ID of game logic
	 * @param _nPlayerID: uint32_t ID of player who search game table
	 * @param _nCount: uint32_t count tables with requested parameters
	 * @param _pvecPrms: TVecFindPrms* requested parameters for tables
	 * @param _pvecRes: TVecUINT game tables
	 * @return FALSE if game tables wasn't founded, TRUE else
	 */
	
	bool Find(uint32_t _nLogicID, uint32_t _nPlayerID, uint32_t _nCount, const TVecFindPrms* _pvecPrms = NULL, TVecUINT* _pvecRes = NULL);

	/**
	 * @brief Get ID of password field in tbParamList mySQL table
	 * @return uint32_t
	 */	
	
	uint32_t GetPasswordID() { return m_nPasswordID; }
		
	/**
	 * @brief Method return patameters of selected game table
	 * @param _nLogicID: uint32_t ID of game logic
	 * @param _nTableID: uint32_t ID of selected game table
	 * @param _vecParIDs: TVecUINT IDs of needed parameters
	 * @param _vecPrms: TVecPrms* parameters of game table
	 * @return FALSE if _vecPrms contain not valid parameter ID, TRUE else
	 */
	
	bool GetTableParams(int _nLogicID, int _nTableID, const TVecUINT& _vecParIDs, TVecPrms *_vecPrms);
	
	/**
	 * @brief Method return ID of last inserted into database game table
	 * @return ID of last inserted into database game table
	 */

	uint32_t LastInsertId();
	
	/**
	 * @brief Method checks actual PlayerID take part in any game
	 * @param _nLogicID: uint32_t ID of game logic
	 * @param _nPlayerID: uint32_t ID of player
	 * @param vecRes: TVecUINT* IDs of game tables where actual player take part in
	 * @return TRUE if some game tables was founded, FALSE else
	 */
	
	bool GetMyTable(int _nLogicID, int _nPlayerID, TVecUINT* vecRes);
	
	/**
	 * @brief Метод возвращает ID случаного стола среди открытых со статусом 2  
	 * @param _nLogicID: uint32_t ID игровой логики
	 * @param _nPlayerID: uint32_t ID игрока, который ищет случайный стол
	 * @param _pvecPrms: TVecFindPrms вектор параметров стола для команды FIND
	 * @return ID of random game table
	 */

    uint32_t RandomOpponent(uint32_t _nLogicID, uint32_t _nPlayerID, const TVecFindPrms* _pvecPrms = NULL);
    
    CSqlLogicList* getSqlLogicList() { return &m_sqlLogicList; }

private:
	
	/**
	 * @brief Method for checking validity of game parameters
	 * @param _vecPrms: const TVecPrms parameters of game
	 * @return TRUE if parameters if valid, FALSE else
	 */
	
	bool CheckParams(const TVecPrms &_vecPrms);
	
	/**
	 * @brief Method for searching free game tables
	 * @param _nLogicID: uint32_t ID of game logic
	 * @param _nPlayerID: uint32_t ID of player who search the free game table
	 * @param vecRes: TVecUINT* IDs of free game tables
	 * @return TRUE if searching done, FALSE else
	 */

	bool FindEmpty(int _nLogicID, int _nPlayerID, TVecUINT* vecRes );
	/**
	 * @brief Method get database table for actual logic
 	 * @param _nLogicID: uint32_t ID of game logic
	 * @param _pRes: CSqlTable* database table for actual logic
	 * @return TRUE if database table was founded, FALSE else
	 */

    bool GetLogicTable(int _nLogicID, CSqlTable* _pRes);

    bool GetAllTables(uint32_t _nLogicID, TVecUINT* _pvecRes);


private:

	CSqlLogicList 	m_sqlLogicList;

	int m_nLastId, m_nPasswordID;
};

#endif 
