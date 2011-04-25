<?php
	
	$DbServerAddr = 'localhost';
	$DbServerName = 'WapServer3';
	$DbServerPwd =  'win74';
	$DbName = 'WapServer3DB';
	
	$DbErrorConnect = 'Can\'t connect to DB (' . $DbName. '): ';
	$DbErrorSelect = 'Can\'t select DB (' . $DbName. '): ';


	$Pages = array (
	  array ("ID" => "Main",     "Name" => "Главная страница", "Menu" => true,  "Link" => "welcome.php" ),
	  array ("ID" => "Download", "Name" => "Скачать игру",	   "Menu" => true, "Link" => "download.php"),
	  array ("ID" => "Find",     "Name" => "Поиск игроков",	   "Menu" => true, "Link" => "find.php"),
	  array ("ID" => "Rules",    "Name" => "Правила игры",	   "Menu" => true, "Link" => "rules.php"),
	  array ("ID" => "Rating",   "Name" => "Рейтинг",	       "Menu" => true, "Link" => "rating.php"),
	  array ("ID" => "About",    "Name" => "О нас",	           "Menu" => true, "Link" => "about.php"),
	  array ("ID" => "Authorization", "Name" => "Авторизация", "Menu" => false, "Link" => "authorization.php")
	  ); 


	function OpenPage($pageID)
	{
		global $Pages;

		for ($i = 0; $i < count($Pages); $i++) 
		{
			if($Pages[$i]["ID"] == $pageID)
			{
    			$name = $Pages[$i]["Name"];
			}
		}

		printf("<html>\n");
		printf("<head>\n");
		printf("<title>%s</title>\n", $name);
		printf("<link rel='stylesheet' type='text/css' href='style/style.css' />\n");
		printf("</head>\n");
		printf("<meta http-equiv='content-type' content='text/html; charset=UTF-8'>\n");
		printf("<body class='wrapper'>\n");
	} 

	function MainMenu($selected)
	{

		global $Pages;

		printf("<ul class='tabs'>\n");

		for ($i = 0; $i < count($Pages); $i++) 
		{
			if($Pages[$i]["Menu"])
			{
				$class = ($Pages[$i]["ID"] == $selected) ? 'tab_selected' : 'tab';
	    		printf("<li class=%s> <a href=%s>%s</a> </li>\n", $class, $Pages[$i]["Link"], $Pages[$i]["Name"]);
			}
		}
		printf("</ul>\n");

		printf("<div class='hr'></div>\n");

	}

	function ClosePage()
	{
		printf("</body>\n");
		printf("</html>\n");
	} 

?> 
