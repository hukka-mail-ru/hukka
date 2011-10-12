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


DROP TABLE IF EXISTS tbPIN;
CREATE TABLE tbPIN
(
  GUID INT UNSIGNED NOT NULL AUTO_INCREMENT,
  PIN VARCHAR(50) NOT NULL,
  FixedSum INT UNSIGNED DEFAULT '0',
  PRIMARY KEY(GUID)
);

INSERT INTO tbPIN (PIN, FixedSum) VALUES ('1100', 100);
INSERT INTO tbPIN (PIN, FixedSum) VALUES ('1200', 200);
INSERT INTO tbPIN (PIN, FixedSum) VALUES ('1300', 300);
INSERT INTO tbPIN (PIN, FixedSum) VALUES ('1400', 400);
INSERT INTO tbPIN (PIN, FixedSum) VALUES ('1500', 500);
INSERT INTO tbPIN (PIN, FixedSum) VALUES ('1600', 600);
INSERT INTO tbPIN (PIN, FixedSum) VALUES ('1700', 700);
INSERT INTO tbPIN (PIN, FixedSum) VALUES ('1800', 800);
INSERT INTO tbPIN (PIN, FixedSum) VALUES ('1900', 900);

SELECT * FROM tbPIN;



delimiter //

DROP PROCEDURE IF EXISTS AddToBalance;
CREATE PROCEDURE AddToBalance (IN player INT, IN val INT)
sproc:BEGIN

DECLARE curBalance INT;

select Balance into curBalance from tbAccount where PlayerID = player;

update tbAccount set Balance=curBalance+val where PlayerID=player;

END;



DROP FUNCTION IF EXISTS ReplenishBalance;
CREATE FUNCTION ReplenishBalance (player INT, PinValue VARCHAR(50))
returns INT 
DETERMINISTIC
BEGIN

DECLARE fixSum INT;
set fixSum = -1;

select FixedSum into fixSum from tbPIN where PIN = PinValue;
if fixSum > 0 THEN 
	call AddToBalance(player, fixSum);
	delete from tbPIN where PIN = PinValue;
end if;

return fixSum;
END;
