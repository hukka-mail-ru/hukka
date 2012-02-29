ID=47
DISCOUNTED_PRICE=2090

rm change.sql
touch change.sql

echo "Call ClockShop.ChangeNewOffer($ID, $DISCOUNTED_PRICE)" > change.sql

mysql -u root -proot --default-character-set=utf8 < change.sql


