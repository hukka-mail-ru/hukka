

<html>
 <head>
  <title>Главная страница</title>
 </head>

  <meta http-equiv="content-type" content="text/html; charset=UTF-8">

 <body>



<?php
	session_start();
	include 'defines.php';
	$lnk = mysql_connect($DbServerAddr, $DbServerName, $DbServerPwd) or die ($DbErrorConnect . mysql_error());
	mysql_select_db($DbName, $lnk) or die ($DbErrorSelect . mysql_error());


	// WELCOME
	if($_SESSION['authorized'] == true)
	{
		printf("<h2>Добро пожаловать, %s!</h2>\n", $_SESSION['UserName']);		
	}
	else
	{
		printf("<h2>Добро пожаловать!</h2>\n");
		printf("<a href=auth.php>Вход</a><br>\n");
		printf("<a href=reg.php>Регистрация</a><br>\n");
	}

	// USER RATING
	if($_SESSION['authorized'] == true)
	{
		$result = mysql_query("SELECT Rating, Available FROM tbChessRating 
							   WHERE PlayerID = " . $_SESSION['UserID']) or die("Invalid query: " . mysql_error());
		
		$row = mysql_fetch_array($result, MYSQL_ASSOC);
		if($row['Available'] == 0)
		{
			echo 'Ваш рейтинг пока еще не доступен. <a href=rating.php>Как его включить?</a><br>';
		}
		else
		{
			echo 'Ваш рейтинг: ' . $row['Rating'];
		}
	}

	// TOP 10 TABLE 
	echo '<br>';
	echo '<br>';
	echo 'Десятка лучших игроков:<br>';
	echo '<br>';
	printf("\n");
	$result = mysql_query("SELECT wsUsers.User, tbChessRating.Rating FROM wsUsers 
                           INNER JOIN tbChessRating ON wsUsers.GUID = tbChessRating.PlayerID 
						   WHERE tbChessRating.Available = 1
                           ORDER by tbChessRating.Rating DESC
						   LIMIT 10") or die("Invalid query: " . mysql_error());

	echo '<table border=2>';
    echo '<td> Место </td>  <td> Игрок </td> <td> Рейтинг </td>';  

	$i = 1;
	while ($row = mysql_fetch_array($result, MYSQL_NUM)) 
    {
		if($_SESSION['authorized'] == true && $row[0] == $_SESSION['UserName'])
			 $bgcolor = "red";
		else
			 $bgcolor = "white";

		printf ("<tr bgcolor=%s>", $bgcolor);
		printf ("<td> %s </td> <td> %s </td> <td> %s </td>\n",  $i, $row[0], $row[1]);  
	    echo '</tr>';
		$i++;
	}
    echo '</table>';

//	$num_rows = mysql_num_rows($result);
//	echo "$num_rows Rows\n";

?> 

<br>
<a href=howto_enable_rating.php>Как включить мой рейтинг</a><br>

</body>
</html>

