USE WapServer3DB;


DROP TABLE IF EXISTS tbLogicList;
CREATE TABLE tbLogicList 
(
      LogicId INT UNSIGNED NOT NULL, 
      LogicName VARCHAR(50) NOT NULL, 
      PRIMARY KEY(LogicId),
      UNIQUE INDEX (LogicName)
) ROW_FORMAT=fixed; 

INSERT INTO tbLogicList  (LogicId, LogicName) VALUES (1,'Gammon');
INSERT INTO tbLogicList  (LogicId, LogicName) VALUES (2,'Chess');

#DESCRIBE tbLogicList;
SELECT * FROM tbLogicList;




DROP TABLE IF EXISTS tbParamList;
CREATE TABLE tbParamList
(
  ParamID INT UNSIGNED NOT NULL, 
  ParamName VARCHAR(50) NOT NULL, 
  Min INT,
  MAX INT,
  IsPassword BOOLEAN,
  IsPlayerID BOOLEAN,
  PRIMARY KEY(ParamID)
) ROW_FORMAT=FIXED;

INSERT INTO tbParamList (ParamID, ParamName, Min, Max, IsPassword, IsPlayerID) VALUES (0, 'IDPlayer0', NULL, NULL, 0, 1);
INSERT INTO tbParamList (ParamID, ParamName, Min, Max, IsPassword, IsPlayerID) VALUES (1, 'IDPlayer1', NULL, NULL, 0, 1);
INSERT INTO tbParamList (ParamID, ParamName, Min, Max, IsPassword, IsPlayerID) VALUES (2, 'Password', NULL, NULL, 1, 0);
INSERT INTO tbParamList (ParamID, ParamName, Min, Max, IsPassword, IsPlayerID) VALUES (3, 'Time2Step', 0, 21600, 0, 0);
INSERT INTO tbParamList (ParamID, ParamName, Min, Max, IsPassword, IsPlayerID) VALUES (4, 'Time2Game', 0, 21600, 0, 0);
INSERT INTO tbParamList (ParamID, ParamName, Min, Max, IsPassword, IsPlayerID) VALUES (5, 'MinRating', NULL, NULL, 0, 0);
INSERT INTO tbParamList (ParamID, ParamName, Min, Max, IsPassword, IsPlayerID) VALUES (6, 'MaxRating', NULL, NULL, 0, 0);
INSERT INTO tbParamList (ParamID, ParamName, Min, Max, IsPassword, IsPlayerID) VALUES (7, 'Bet', NULL, NULL, 0, 0);

#DESCRIBE tbParamList;
SELECT * FROM tbParamList;


