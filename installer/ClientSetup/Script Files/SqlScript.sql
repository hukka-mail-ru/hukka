--------------------------------------------------------------------------------------------------
--                                                                            
--  File Name:   sqlscript.sql                                   
--                                                                            
--  Description: Blank SQL script                                          
--                                                                            
--  Comments:    This blank script is intended for advanced users. 
--               To create a script from an existing database with step-by-step  
--		         instructions, use the Database Import Wizard. 
--                                                                                                               
---------------------------------------------------------------------------------------------------

USE master
GO

-- drop database 
IF EXISTS (SELECT name FROM sys.databases WHERE name = N'TUEV_SUED')
BEGIN
	print('--- Dropping TUEV_SUED Database -----');
	
	ALTER DATABASE TUEV_SUED SET SINGLE_USER WITH ROLLBACK IMMEDIATE    
	
	DROP DATABASE TUEV_SUED
END  

GO
CREATE DATABASE TUEV_SUED COLLATE Latin1_General_CI_AS
GO