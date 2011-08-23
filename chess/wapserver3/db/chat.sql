delimiter //



DROP TABLE IF EXISTS tbChessChat;
CREATE TABLE tbChessChat
(
      TableID INT UNSIGNED,
      Msg TEXT NOT NULL,
      CreateTime TIMESTAMP	
) ROW_FORMAT=fixed;



DROP TABLE IF EXISTS tbChessChatUserOnline;
CREATE TABLE tbChessChatUserOnline
(
      PlayerID INT UNSIGNED NOT NULL DEFAULT '0',
      TableID INT UNSIGNED DEFAULT '0',
      PRIMARY KEY(PlayerID)
);


####################################################################################################################### 
#  Stores the given "message" in tbChessChat, and erases old entries (remaining not more then "maxEntries").
#######################################################################################################################

DROP PROCEDURE IF EXISTS AddToHistory;
CREATE PROCEDURE AddToHistory (IN tab INT, IN message TEXT, IN maxEntries INT)
sproc:BEGIN

DECLARE entries INT;
DECLARE table_exist INT;
DECLARE time TIMESTAMP;


# Don't add the message to History, if the table has been deleted
select COUNT(*) into table_exist from tbChessTableList where TableId = tab;
IF table_exist = 0 THEN 
LEAVE sproc;
END IF;


INSERT INTO tbChessChat(TableId, Msg) VALUES (tab, message);

select COUNT(*) into entries  from tbChessChat where TableId = tab;


WHILE entries > maxEntries 
DO
	select CreateTime into time from tbChessChat where TableId = tab order by CreateTime limit 1;

	delete from tbChessChat where (CreateTime = time and TableId = tab);

	select COUNT(*)  into entries  from tbChessChat where TableId = tab;

END WHILE;

END;
