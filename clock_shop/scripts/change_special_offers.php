<?php

	$ids    = array(9,   12,   13,   14,   15,   16);
	$prices = array(1990, 1990, 1990, 1990, 1990, 1990);
	

	//connect to the database
	mysql_connect("localhost", "hukka", "777") or die (db_error());
	mysql_select_db("ClockShop") or die (db_error());


	$q = mysql_query("SELECT productID FROM SS_special_offers") or die (db_error());	
	while ($row = mysql_fetch_row($q))
	{
		$offerID = $row[0];

		$qq = mysql_query("SELECT list_price FROM SS_products WHERE productID=$offerID") or die (db_error());
		while ($rrow = mysql_fetch_row($qq))
		{
			$list_price = $rrow[0];
			printf("UPDATE SS_products SET price=$list_price WHERE productID=$offerID; \n");
		}
	}

	printf("\n");
	printf("DELETE FROM SS_special_offers;\n\n");

	foreach ($ids as $i => $value) 
	{
		printf("INSERT INTO SS_special_offers (productID) VALUES ($ids[$i]);\n");
		printf("UPDATE SS_products SET price=$prices[$i] WHERE productID=$ids[$i]; \n\n");
	}
	

?>
