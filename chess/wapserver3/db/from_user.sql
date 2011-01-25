USE WapServer3DB;

CREATE TABLE IF NOT EXISTS wsUsers 
(
	User VARCHAR(50) NOT NULL, 
	Password VARCHAR(50) NOT NULL,
	GUID INT UNSIGNED NOT NULL AUTO_INCREMENT, 
	PRIMARY KEY(GUID),
	UNIQUE INDEX (User)
) AUTO_INCREMENT = 100;

DESCRIBE wsUsers;
INSERT INTO wsUsers (GUID,User,Password) VALUES (1,'srv',PASSWORD('srv'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (2,'reg',PASSWORD('reg'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (3,'cht',PASSWORD('cht'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (4,'tbm',PASSWORD('tbm'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (5,'lgm',PASSWORD('lgm'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (6,'chs',PASSWORD('chs'));
INSERT INTO wsUsers (GUID,User,Password) VALUES (33,'lxo',PASSWORD('lxo'));
