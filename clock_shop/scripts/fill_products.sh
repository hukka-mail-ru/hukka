mysqldump -u hukka -p777 ClockShop SS_products > dump.sql

perl insert_products.pl artikul.txt > temp.sql

mysql -u root -proot --default-character-set=utf8 < temp.sql

rm temp.sql
rm dump.sql
