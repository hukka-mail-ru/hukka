#!/usr/bin/perl

# Usage : perl insert.pl artikul.txt

print "DELETE FROM ClockShop.SS_products;\n\n";

while ($line = <>) # read a file
{ 
  #  print "a new line\n";


    if($line =~ m/([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+/)
    {
	$code         = $1;
	$size         = $2;
	$categoryID   = $3;
	$price        = $4;
	$stock_price  = $5;
	$stock        = $6;
	$name         = $7;
	$color        = $8;
	
	$color = ($color =~ "NA") ? "" : "Цвет: $color. ";

	# skip 'Not in stock'
	if($stock =~ "да")
	{
		print "INSERT INTO ClockShop.SS_products (product_code, categoryID, name, brief_description, description, 
		       enabled, customers_rating, customer_votes, in_stock, items_sold,
		       Price, list_price, stock_price,
		       thumbnail, picture, big_picture)
		       VALUES (
		       '$code', $categoryID, '$name', 'Размер: $size', 
	               'Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. $colorРазмер: $size',
		       1, 0, 0, 1, 0,
		       $price, $price, $stock_price,
		       'thumbnail/$code.jpg', 'small/$code.jpg', 'big/$code.jpg'); \n\n"
	}
    }


}
