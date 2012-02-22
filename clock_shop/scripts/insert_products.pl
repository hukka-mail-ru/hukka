#!/usr/bin/perl

# Usage : perl insert.pl artikul.txt

#print "DELETE FROM ClockShop.SS_products;\n\n";

while ($line = <>) # read a file
{ 
  #  print "a new line\n";


    if($line =~ m/([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+/)
    {
	$code         = $1;
	$size         = $2;
	$categoryID   = $3;
	$list_price   = $4;
	$discount_price = $5;
	$stock_price  = $6;
	$stock        = $7;
	$name         = $8;
	$color        = $9;
	
	$colorText = ($color =~ "-") ? "" : "Цвет: $color. ";

	$price = ($discount_price =~ "-") ? $list_price : $discount_price;


	$brief_description = "Размер: $size";
	$description   	= "Керамические часы ручной работы. Бесшумный механизм. Питание от двух батареек АА. $colorTextРазмер: $size";
	$enabled	= 1;
	$customers_rating = 0; 
	$customer_votes	= 0; 
	$in_stock	= 1;
	$items_sold	= 0;
	$thumbnail     	= "thumbnail/$code.jpg";
	$picture       	= "small/$code.jpg";
	$big_picture   	= "big/$code.jpg";


	# does this code already exist?
	$codeExists = 0;
	$DUMP = "dump.sql";
	open(DUMP) or die("Could not open dump.sql");
	foreach $line (<DUMP>) 
        {
	    if(index($line, $code) != -1) # finds $code in the $line
	    {
		$codeExists = 1;
            }
	}
	close(DUMP);


	# skip 'Not in stock'
	if($stock =~ "да")
	{
		if($codeExists == 1)
		{

			print "UPDATE ClockShop.SS_products SET 
				categoryID = $categoryID, 
				name = '$name',
				brief_description = '$brief_description', 
				description = '$description', 
				color = '$color',
		       		enabled = $enabled, 
				customers_rating = $customers_rating, 
				customer_votes = $customer_votes, 
				in_stock = $in_stock, 
			        Price = $price, 
                                stock_price = $stock_price,
				list_price = $list_price,
			        thumbnail = '$thumbnail', 
                                picture = '$picture', 
                                big_picture = '$big_picture'
			       WHERE 
  			        product_code = '$code'; \n\n"

		}
		else
		{
			print "INSERT INTO ClockShop.SS_products (product_code, categoryID, name, brief_description, description, color,
		       		enabled, customers_rating, customer_votes, in_stock, items_sold,
			       Price, list_price, stock_price, list_price,
			       thumbnail, picture, big_picture)
			       VALUES (
			       '$code', $categoryID, '$name', '$brief_description', '$description', '$color'
			       $enabled, $customers_rating, $customer_votes, $in_stock, $items_sold,
			       $price, $list_price, $stock_price, $list_price,
			       '$thumbnail', '$picture', '$big_picture'); \n\n"
		}
	}
    }


}
