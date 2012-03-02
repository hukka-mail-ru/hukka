
perl fill_products.pl products.txt > temp.sql

cat temp.sql | grep "CALC"

mysql -u root -proot --default-character-set=utf8 < temp.sql

./fill_offers.sh

# rm temp.sql
# rm dump.sql
