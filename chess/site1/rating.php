<?php
	include 'defines.php';

	OpenPage("Рейтинг");
	session_start();

	printf("<div class='header'>Рейтинг</div>\n");
	

	if($_SESSION['authorized'] == true)
	{
		$lnk = mysql_connect($DbServerAddr, $DbServerName, $DbServerPwd) or die ($DbErrorConnect . mysql_error());
		mysql_select_db($DbName, $lnk) or die ($DbErrorSelect . mysql_error());

		$result = mysql_query("SELECT Rating, Available FROM tbChessRating 
							   WHERE PlayerID = " . $_SESSION['UserID']) or die("Invalid query: " . mysql_error());
		
		$row = mysql_fetch_array($result, MYSQL_ASSOC);
		if($row['Available'] == 0)
		{
			printf("<div class='wrapper'> Ваш рейтинг пока еще не доступен. <a href=enable_rating.php>Как его включить?</a><br> </div>\n");
		}
		else
		{
			printf("<div class='wrapper'> Ваш рейтинг: %s<br> баллов.</div>\n", $row['Rating']);

    		$result = mysql_query("select count(*) from tbChessRating WHERE Rating >= " . $row['Rating'] . " AND Available=1");
			$place = mysql_fetch_array($result, MYSQL_NUM);

    		$result = mysql_query("select count(*) from tbChessRating WHERE Available=1");
			$playersNumber = mysql_fetch_array($result, MYSQL_NUM);

			printf("<div class='wrapper'> %s место среди %s участников</div>\n", $place[0], $playersNumber[0]);
		}


	}
	else
	{
		printf("<div class='wrapper'> Для просмотра Вашего рейтинга необходимо <a href=auth.php>войти в систему</a>.  </div>\n");
	}

	printf("<br><br>");

	printf("<div class='header'>Правила начисления рейтинга</div>\n");

	printf("<div class='wrapper'> \n");
	printf("<ul>\n");
    printf("<li>При активации рейтинга Вам автоматически начисляется 1000 баллов. </li>\n");
	printf("<li>За каждую победу к вашему рейтингу прибавляется одна десятая рейтинга побежденного соперника. Таким образом, вам выгодно сражаться с сильными игроками. Например, если Вы победите игрока с рейтингом в 2 раза больше Вашего, Ваш собственный рейтинг увеличится на 20%%.</li>\n");
	printf("<li>За каждое поражение ваш рейтинг уменьшается на 10%%, независимо от того, кому Вы проиграли.</li>\n");
	printf("<li>При ничье рейтинг считается по более сложной формуле. Если, например, вы сыграли вничью с игроком с таким же рейтингом как у Вас, то Ваш рейтинг не изменится. Но если Вы сыграли вничью с игроком, чей рейтинг в 2 раза больше Вашего, то Ваш рейтинг увеличится на 10%%.\n");
	printf("<li>Если в партии было сделано менее 5 ходов, и она была завершена, Ваш рейтинг не изменится.</li>\n");
	printf("</ul>\n");
	printf("</div>\n");



	ClosePage();
?> 
