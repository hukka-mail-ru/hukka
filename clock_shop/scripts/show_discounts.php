<?php

	//connect to the database
	mysql_connect("localhost", "hukka", "777") or die (db_error());
	mysql_select_db("ClockShop") or die (db_error());



	$q = mysql_query("SELECT price, list_price, productID, name, product_code FROM SS_products") or die (db_error());
	$i  = 1;
	while ($row = mysql_fetch_row($q))
	{
		$price = $row[0];
		$list_price = $row[1];
		$productID = $row[2];
		$name = $row[3];
		$product_code = $row[4];

		if($price != $list_price)
		{
			$discount = $list_price - $price;
			printf("$i:\t$productID\t$price\t$list_price\t($discount)\t$product_code\t$name\n");
			$i++;
		}
	}

?>
