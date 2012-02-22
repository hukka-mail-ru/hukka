mkdir -p category
mkdir -p thumbnail
mkdir -p small


cd big

for f in *.jpg
do
	echo $f
	convert $f -resize 70x70 ../category/$f
	convert $f -resize 170x170 ../thumbnail/$f
	convert $f -resize 250x250 ../small/$f
done
