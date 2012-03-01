<?php

function removeDiscounts($table)
{
	$q = mysql_query("SELECT productID FROM $table") or die (db_error());	
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
}


	// загружаем текст из файла
	$text = join('',file('offers.txt'));

	// разбиваем по словам
	$words = preg_split("/\s+/s",$text);

	// выводим результаты
	//print_r($words);

	$key	= array();
	$ids    = array();
	$prices = array();

	$ii = 0;
	for ($i = 0; $i < count($words) - 3; $i = $i+3) 
	{
		$key[$ii] = $words[$i];
		$ids[$ii] = $words[$i + 1];
		$prices[$ii] = $words[$i + 2];
		$ii++;
	}

//print_r($prices);
	

	//connect to the database
	mysql_connect("localhost", "hukka", "777") or die (db_error());
	mysql_select_db("ClockShop") or die (db_error());


	// remove discounts 
	removeDiscounts("SS_special_offers");
	removeDiscounts("SS_new_offers");

	// delete old offers
	printf("\n");
	printf("DELETE FROM SS_special_offers;\n");
	printf("DELETE FROM SS_new_offers;\n\n");

	// create updated offers
	foreach ($ids as $i => $value) 
	{
		if($key[$i] == "special")
		{
			printf("INSERT INTO SS_special_offers (productID) VALUES ($ids[$i]);\n");
		}
		if($key[$i] == "new")
		{
			printf("INSERT INTO SS_new_offers (productID) VALUES ($ids[$i]);\n");
		}

		$qq = mysql_query("SELECT list_price FROM SS_products WHERE productID=$ids[$i]") or die (db_error());
		while ($rrow = mysql_fetch_row($qq))
		{
			$list_price = $rrow[0];
		}

		$discouted_price = $list_price - $prices[$i];
		printf("UPDATE SS_products SET price=$discouted_price WHERE productID=$ids[$i]; \n\n");

	}

?>
