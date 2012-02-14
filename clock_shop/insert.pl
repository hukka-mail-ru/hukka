#!/usr/bin/perl

# Usage : perl insert.pl artikul.txt

print "DELETE FROM ClockShop.SS_products;\n\n";

while ($line = <>) # read a file
{ 
  #  print "a new line\n";


    if($line =~ m/([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+/)
    {

	print "INSERT INTO ClockShop.SS_products (product_code, categoryID, name, brief_description, description, 
	       enabled, customers_rating, customer_votes, in_stock, items_sold,
	       Price, list_price,
	       thumbnail, picture, big_picture)
	       VALUES (
	       '$1', $3, '$5', 'brief_description', 'description: $2',
	       1, 0, 0, 1, 0,
	       1999, 1999,
	       'thumbnail/$1.jpg', 'small/$1.jpg', 'big/$1.jpg'); \n\n"
    }


}
