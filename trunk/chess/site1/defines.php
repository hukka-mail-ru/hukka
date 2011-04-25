<?php
	
	$DbServerAddr = 'localhost';
	$DbServerName = 'WapServer3';
	$DbServerPwd =  'win74';
	$DbName = 'WapServer3DB';
	
	$DbErrorConnect = 'Can\'t connect to DB (' . $DbName. '): ';
	$DbErrorSelect = 'Can\'t select DB (' . $DbName. '): ';


	$Pages = array (
	  "Main"     => array ("Name" => "Главная страница",   "Menu" => true,  "Link" => "welcome.php" ),
	  "Download" => array ("Name" => "Скачать игру",	   "Menu" => true,  "Link" => "download.php"),
	  "Find"     => array ("Name" => "Поиск игроков",	   "Menu" => true,  "Link" => "find.php"),
	  "Rules"    => array ("Name" => "Правила игры",	   "Menu" => true,  "Link" => "rules.php"),
	  "Rating"   => array ("Name" => "Рейтинг",	           "Menu" => true,  "Link" => "rating.php"),
	  "About"    => array ("Name" => "О нас",	           "Menu" => true,  "Link" => "about.php"),
	  "Auth"     => array ("Name" => "Авторизация",        "Menu" => false, "Link" => "authorization.php")
	  ); 


	function OpenPage($pageID)
	{
		global $Pages;

		printf("<html>\n");
		printf("<head>\n");
		printf("<title>%s</title>\n", $Pages[$pageID]["Name"]);
		printf("<link rel='stylesheet' type='text/css' href='style/style.css' />\n");
		printf("</head>\n");
		printf("<meta http-equiv='content-type' content='text/html; charset=UTF-8'>\n");
		printf("<body class='wrapper'>\n");
	} 

	function MainMenu($selectedPageID)
	{

		global $Pages;

		printf("<ul class='tabs'>\n");
		foreach ($Pages as &$page)
		{
			if($page["Menu"])
			{
				$class = ($page == $Pages[$selectedPageID]) ? 'tab_selected' : 'tab';
	    		printf("<li class=%s> <a href=%s>%s</a> </li>\n", $class, $page["Link"], $page["Name"]);
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
