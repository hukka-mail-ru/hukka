#!/bin/bash

php change_offers.php > change.sql

mysql -u root -proot --default-character-set=utf8 --database=ClockShop < change.sql


