USE WapServer3DB;

#mysql> describe tbGammonTableList;
#+-----------------+----------------------+------+-----+---------+----------------+
#| Field           | Type                 | Null | Key | Default | Extra          |
#+-----------------+----------------------+------+-----+---------+----------------+
#| TableID         | int(10) unsigned     |      | PRI | NULL    | auto_increment |
#| State           | tinyint(3) unsigned  |      |     | 1       |                |
#| IDPlayer0       | int(10) unsigned     |      |     | 0       |                |
#| IDPlayer1       | int(10) unsigned     | YES  |     | NULL    |                |
#| password        | varchar(50)          | YES  |     | NULL    |                |
#| Time2Step       | int(10) unsigned     | YES  |     | 600     |                |
#| Time2Game       | int(10) unsigned     | YES  |     |         |                |
#| MinRating       | int(10) unsigned     | YES  |     | NULL    |                |
#| MaxRating       | int(10) unsigned     | YES  |     | NULL    |                |
#| StepNum         | smallint(5) unsigned | YES  |     | NULL    |                |
#| XPlayer         | int(10) unsigned     | YES  |     | NULL    |                |
#| CurPlayer       | int(10) unsigned     | YES  |     | NULL    |                |
#| DrawState       | tinyint(3) unsigned  |      |     | 0       |                |
#| PlayerStepTime  | int(10) unsigned     | YES  |     | NULL    |                |
#| Player0GameTime | int(10) unsigned     | YES  |     | 0       |                |
#| Player1GameTime | int(10) unsigned     | YES  |     | 0       |                |
#| FieldState      | blob                 | YES  |     | NULL    |                |
#+-----------------+----------------------+------+-----+---------+----------------+

DROP TABLE IF EXISTS tbChessTableList;

CREATE TABLE IF NOT EXISTS tbChessTableList
(
  TableID INT UNSIGNED NOT NULL AUTO_INCREMENT,
  State TINYINT UNSIGNED NOT NULL DEFAULT '1',
  IDPlayer0 INT UNSIGNED NOT NULL DEFAULT '0',
  IDPlayer1 INT UNSIGNED NULL,
  password VARCHAR(50) NULL,
  Time2Step INT UNSIGNED NULL DEFAULT '600',
  Time2Game INT UNSIGNED NULL,
  MinRating INT UNSIGNED NULL,
  MaxRating INT UNSIGNED NULL,
  StepNum SMALLINT UNSIGNED NOT NULL DEFAULT '0',
  XPlayer INT UNSIGNED NOT NULL DEFAULT '0',
  CurPlayer INT UNSIGNED NULL,
  DrawState TINYINT UNSIGNED NOT NULL DEFAULT '0',
  PlayerStepTime INT UNSIGNED NULL,
  Player0GameTime INT UNSIGNED NULL DEFAULT '0',
  Player1GameTime INT UNSIGNED NULL DEFAULT '0',
  OpGameTime INT UNSIGNED NULL,
  FieldState BLOB NULL,
  GameLog TEXT NULL,
  PRIMARY KEY( TableID )
);

DESCRIBE tbChessTableList;
SELECT * FROM tbChessTableList;

CREATE TABLE IF NOT EXISTS tbChessRating
(
  PlayerID INT UNSIGNED NOT NULL DEFAULT '0',
  Rating INT UNSIGNED NOT NULL DEFAULT '0',
  PRIMARY KEY(PlayerID)
);

DESCRIBE tbChessRating;
