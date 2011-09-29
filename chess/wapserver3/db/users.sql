USE WapServer3DB;

DROP TABLE IF EXISTS wsUsers;
CREATE TABLE wsUsers 
(
	User VARCHAR(50) NOT NULL, 
	Password VARCHAR(50) NOT NULL,
	GUID INT UNSIGNED NOT NULL AUTO_INCREMENT, 
	PRIMARY KEY(GUID),
	UNIQUE INDEX (User)
) AUTO_INCREMENT = 100;

#DESCRIBE wsUsers;

INSERT INTO wsUsers (GUID,User,Password) VALUES (1,'srv',PASSWORD('srv'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (2,'reg',PASSWORD('reg'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (3,'cht',PASSWORD('cht'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (4,'tbm',PASSWORD('tbm'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (5,'lgm',PASSWORD('lgm'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (6,'chs',PASSWORD('chs'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (100,'test','test');
INSERT INTO wsUsers (GUID,User,Password) VALUES (101,'test1','test');
INSERT INTO wsUsers (GUID,User,Password) VALUES (102,'robot','test');

SELECT * FROM wsUsers;



DROP TABLE IF EXISTS tbAccount;
CREATE TABLE tbAccount
(
  PlayerID INT UNSIGNED NOT NULL DEFAULT '0',
  Balance INT UNSIGNED NOT NULL DEFAULT '0',
  PRIMARY KEY(PlayerID)
);

INSERT INTO tbAccount (PlayerID, Balance) VALUES (6, 0);
INSERT INTO tbAccount (PlayerID, Balance) VALUES (100, 500);
INSERT INTO tbAccount (PlayerID, Balance) VALUES (101, 700);

SELECT * FROM tbAccount;




delimiter //
DROP PROCEDURE IF EXISTS AddToBalance;
CREATE PROCEDURE AddToBalance (IN player INT, IN val INT)
sproc:BEGIN

DECLARE curBalance INT;

select Balance into curBalance from tbAccount where PlayerID = player;

update tbAccount set Balance=curBalance+val where PlayerID=player;

END;
