
perl insert_products.pl artikul.txt > temp.sql

cat temp.sql | grep "CALC"

mysql -u root -proot --default-character-set=utf8 < temp.sql

./change_offers.sh

# rm temp.sql
# rm dump.sql
