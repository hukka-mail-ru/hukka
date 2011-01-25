#include "SqlChessTable.h"

CSqlChessTable::CSqlChessTable() :
	CSqlGameTable("tbChessTableList",
	   "TableID INT UNSIGNED NOT NULL AUTO_INCREMENT,\
		State TINYINT UNSIGNED NOT NULL DEFAULT '1',\
		IDPlayer0 INT UNSIGNED NOT NULL,\
		IDPlayer1 INT UNSIGNED NULL,\
		password VARCHAR(50) NULL,\
		Time2Step INT UNSIGNED NOT NULL DEFAULT '300',\
		Time2Game INT UNSIGNED NOT NULL DEFAULT '3600',\
		MinRating INT UNSIGNED NULL,\
		MaxRating INT UNSIGNED NULL,\
		StepNum SMALLINT UNSIGNED NULL,\
		XPlayer INT UNSIGNED NULL,\
		CurPlayer INT UNSIGNED NULL,\
		PlayerStepTime INT UNSIGNED NULL,\
		Player0GameTime INT UNSIGNED NULL DEFAULT '0',\
		Player1GameTime INT UNSIGNED NULL DEFAULT '0',\
		OpGameTime INT UNSIGNED NULL,\
		FieldState TINYBLOB NULL,\
		GameLog TEXT NULL,\
		PRIMARY KEY( TableID )")
{
}

CSqlChessTable::~CSqlChessTable()
{
}
