if [ -z "$1" ] 
then
	echo "Usage: configure.sh NAME"
	exit 1
fi	


perl -i -p -e 's/main/'$1'/g' restart.sh

cd src

perl -i -p -e 's/hello/'$1'/g' main.c

mv main.c $1.c

echo "obj-m	+= $1.o" > Makefile

cd ..

