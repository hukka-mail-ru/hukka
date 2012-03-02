perl fill_categories.pl categories.txt > temp.sql

mysql -u root -proot --default-character-set=utf8 < temp.sql

rm temp.sql

