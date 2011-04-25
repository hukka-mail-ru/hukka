
<?php
	include 'defines.php';

	OpenPage("Find");
	MainMenu("Find");

	printf ("<h2>Поиск</h2>");


	printf ("<form name='find' action='find.php' method=post>");
	printf ("Рейтинг от : <input type=number name=rating_min>");
	printf ("до : <input type=number name=rating_max>");
	printf ("<input type=submit name=send value=Искать>");
	printf ("</form>");


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

	ClosePage();
?>

