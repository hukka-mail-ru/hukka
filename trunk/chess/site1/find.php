<html>
 <head>
  <title>Поиск</title>
 </head>

  <meta http-equiv="content-type" content="text/html; charset=UTF-8">

 <body>

<h2>Поиск</h2>


<form name="find" action="find.php" method=post>
Рейтинг от : <input type=number name=rating_min>
до : <input type=number name=rating_max>
<input type=submit name=send value=Искать>
</form>


<?php
	include 'defines.php';

	if (isset($_POST['rating_min']))
	{
		 $rating_min = $_POST['rating_min']; 
		 if ($rating_min== '') { unset($rating_min);}
	}

	if (isset($_POST['rating_max']))
	{
		 $rating_max = $_POST['rating_max']; 
		 if ($rating_max == '') { unset($rating_max);}
	}

     if (empty($rating_min) or empty($rating_max))
	 {
		  exit;
	 }

    $lnk = mysql_connect($DbServerAddr, $DbServerName, $DbServerPwd) or die ($DbErrorConnect . mysql_error());
	mysql_select_db($DbName, $lnk) or die ($DbErrorSelect . mysql_error());

	$result = mysql_query("SELECT wsUsers.User, tbChessRating.Rating FROM wsUsers 
                           INNER JOIN tbChessRating ON wsUsers.GUID = tbChessRating.PlayerID 
						   WHERE tbChessRating.Available = 1 
                             AND tbChessRating.Rating >= " . $_POST['rating_min'] . 
                           " AND tbChessRating.Rating <= " . $_POST['rating_max'] .
                           " ORDER by tbChessRating.Rating DESC") or die("Invalid query: " . mysql_error());

	echo '<table border=2>';
    echo '<td> Место </td>  <td> Игрок </td> <td> Рейтинг </td>';  
	$i = 1;
	while ($row = mysql_fetch_array($result, MYSQL_NUM)) 
    {
		printf ("<tr> <td> %s </td> <td> %s </td> <td> %s </td> </tr>\n", $i, $row[0], $row[1]);  
		$i++;
	}
    echo '</table>';


?>

</body>
</html>
