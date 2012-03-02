#!/usr/bin/perl

# Usage : perl insert.pl categories.txt

print "DELETE FROM ClockShop.SS_categories;\n\n";

while ($line = <>) # read a file
{ 
  #  print "a new line\n";


    if($line =~ m/([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)$/)
    {
	$categoryID  = $1;
	$parent      = $2;
	$name        = $3;
	$name_de     = $4;
	$picture_code = $5;
	$description = $6;
	

	print "INSERT INTO ClockShop.SS_categories (categoryID, parent, name, name_de, picture, description, products_count, products_count_admin)
	        VALUES ( $categoryID, $parent, '$name', '$name_de', 'category/$picture_code.jpg', '$description', 0, 0); \n\n";
    }


}
