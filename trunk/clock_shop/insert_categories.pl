#!/usr/bin/perl

# Usage : perl insert.pl categories.txt

print "DELETE FROM ClockShop.SS_categories;\n\n";

while ($line = <>) # read a file
{ 
  #  print "a new line\n";


    if($line =~ m/([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+/)
    {

	print "INSERT INTO ClockShop.SS_categories (categoryID, parent, name, picture, description, products_count, products_count_admin)
	        VALUES ( $1, $2, '$3', 'category/$4.jpg', '$5', 0, 0); \n\n";
    }


}
