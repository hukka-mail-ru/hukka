perl insert_categories.pl categories.txt > insert_categories.sql

mysql -u root -proot --default-character-set=utf8 < insert_categories.sql

