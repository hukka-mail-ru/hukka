SHARED=~/temp
TEMP=./temp
SHOP=shop-script-free

rm -rf $SHARED/$SHOP

mkdir $TEMP

echo "Copying to temp"
cp -r ../$SHOP $TEMP

echo "Deleting .svn"
find . -name '*.svn' | xargs rm -rf

echo "Copying to shared"
mv $TEMP/$SHOP $SHARED

rm -rf $TEMP
