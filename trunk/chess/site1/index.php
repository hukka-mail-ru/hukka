<html>
 <head>
  <title>PHP Test</title>
 </head>

  <meta http-equiv="content-type" content="text/html; charset=UTF-8">

 <body>

<h2>Добро пожаловать!</h2>

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

	echo '<table border=2>';
    echo '<td> NAME </td> <td> RATING </td>';  
	while ($row = mysql_fetch_array($result, MYSQL_NUM)) 
    {
	    echo '<tr>';
		printf ("<td> %s </td> <td> %s </td>", $row[0], $row[1]);  
	    echo '</tr>';
	}
    echo '</table>';

//	$num_rows = mysql_num_rows($result);
//	echo "$num_rows Rows\n";

?> 
<br>
Как включить мой рейтинг<br>

</body>
</html>

