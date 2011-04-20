<html>
 <head>
  <title>Главная страница</title>
 </head>

  <meta http-equiv="content-type" content="text/html; charset=UTF-8">

 <body>

<h2>Добро пожаловать!</h2>

<a href=auth.php>Вход</a><br>
<a href=reg.php>Регистрация</a><br>
<br>
Десятка лучших игроков:<br>
<br>

<?php

	$lnk = mysql_connect('localhost', 'WapServer3', 'win74')
		   or die ('Not connected : ' . mysql_error());

	mysql_select_db('WapServer3DB', $lnk) or die ('Can\'t use WapServer3DB : ' . mysql_error());

	$result = mysql_query("SELECT wsUsers.User, tbChessRating.Rating FROM wsUsers 
                           INNER JOIN tbChessRating ON wsUsers.GUID = tbChessRating.PlayerID 
						   WHERE tbChessRating.Available = 1
                           ORDER by tbChessRating.Rating DESC
						   LIMIT 10")
		or die("Invalid query: " . mysql_error());

	// TOP 10 TABLE 
	echo '<table border=2>';
    echo '<td> Место </td>  <td> Игрок </td> <td> Рейтинг </td>';  

	$i = 1;
	while ($row = mysql_fetch_array($result, MYSQL_NUM)) 
    {
	    echo '<tr>';
		printf ("<td> %s </td> <td> %s </td> <td> %s </td>", $i, $row[0], $row[1]);  
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

