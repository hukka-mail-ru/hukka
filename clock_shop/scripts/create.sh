#!/bin/sh

mysql -u root -proot < create_db.sql

mysql -u root -proot < dump.sql
