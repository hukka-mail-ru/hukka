#!/usr/bin/perl

use POSIX;

# Usage : perl insert.pl artikul.txt

$transport = 15; # EUR
$transport_insurance = $transport/10; # EUR, one time of 10 we need to resend
$refuse_insurance = $transport/10; # EUR, one time of 10 customer returns
$gurantee_insurance = $transport/100; # EUR, one time of 100 we need to repair
$kurs = 40; # RUB/EUR
$advertising = 200; # RUB
$my_interest = 500; # RUB	
$discount = 10;# RUB	

$expencies = ($transport + $transport_insurance + $gurantee_insurance + $refuse_insurance) * $kurs + $advertising;
$total_margin = $expencies +  $my_interest;

print "# CALC: transport:\t $transport EUR\n";
print "# CALC: transport insurance:\t $transport_insurance EUR\n";
print "# CALC: refuse insurance:\t $refuse_insurance EUR\n";
print "# CALC: gurantee insurance:\t $gurantee_insurance EUR\n";
print "# CALC: advertising:\t $advertising RUB\n";
print "# CALC: ====================\n";
print "# CALC: kurs:\t $kurs RUB/EUR\n";
print "# CALC: expencies:\t $expencies RUB\n";
print "# CALC: ====================\n";
print "# CALC: my interest:\t $my_interest RUB\n";
print "# CALC: TOTAL MARGIN:\t $total_margin RUB\n";


print "\n\n";
print "DELETE FROM ClockShop.SS_products;\n";
print "ALTER TABLE ClockShop.SS_products AUTO_INCREMENT = 100;\n\n";

while ($line = <>) # read a file
{ 
  #  print "a new line\n";


    if($line =~ m/([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)\t+([^\t]+)$/)
    {
	$code         = $1;
	$size         = $2;
	$categoryID   = $3;
	$price        = $4;
	$stock_price  = $5;
	$stock        = $6;
	$name         = $7;
	$color        = $8;
	

	$price = ceil($stock_price * $kurs + $total_margin);
	$price  = $price + 100 - ($price % 100) - $discount; # round to the nearest 100, discount 10.

	$list_price = $price;

	$brief_description = "Размер: $size";

	$type   	= ($categoryID == 4) ? "Часы из нержавеющей стали" : "Керамические часы";

	$mech		= ($categoryID == 10 || $categoryID == 11 || $categoryID == 12 || $categoryID == 4) ? 
			  "Бесшумный маятниковый механизм" : "Бесшумный кварцевый механизм";

	$description	= "$type, ручная работа. $mech. Гарантия 2 года. Питание от одной батарейки АА. Размер: $size.";
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
			print "INSERT INTO ClockShop.SS_products (product_code, categoryID, name, brief_description, description, color,
		       		enabled, customers_rating, customer_votes, in_stock, items_sold,
			       Price, stock_price, list_price,
			       thumbnail, picture, big_picture)
			       VALUES (
			       '$code', $categoryID, '$name', '$brief_description', '$description', '$color',
			       $enabled, $customers_rating, $customer_votes, $in_stock, $items_sold,
			       $price, $stock_price, $list_price,
			       '$thumbnail', '$picture', '$big_picture'); \n\n"
	 }

    }


}
