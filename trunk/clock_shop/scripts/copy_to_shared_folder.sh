SHARED=~/temp
TEMP=./temp
SHOP=shop-script-free

rm -rf $SHARED/$SHOP

rm -rf $TEMP
mkdir -p $TEMP

echo "Copying to temp"
cp -r ../$SHOP $TEMP

echo "Deleting .svn"
find $TEMP -name '*.svn' | xargs rm -rf

echo "Deleting .tar.gz"
find $TEMP -name '*.tar.gz' | xargs rm -v

echo "Setting properties to connect to hoster.hu"
mv $TEMP/$SHOP/cfg/connect.inc.hoster.php $TEMP/$SHOP/cfg/connect.inc.php
mv $TEMP/$SHOP/includes/database/mysql.hoster.php $TEMP/$SHOP/includes/database/mysql.php

echo "Copying to shared"
mv $TEMP/$SHOP $SHARED

rm -rf $TEMP
