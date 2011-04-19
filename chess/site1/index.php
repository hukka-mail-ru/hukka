<html>
 <head>
  <title>PHP Test</title>
 </head>
 <body>


<?php

	$lnk = mysql_connect('localhost', 'WapServer3', 'win74')
		   or die ('Not connected : ' . mysql_error());

	mysql_select_db('WapServer3DB', $lnk) or die ('Can\'t use WapServer3DB : ' . mysql_error());

	$result = mysql_query("SELECT wsUsers.User, tbChessRating.Rating FROM wsUsers INNER JOIN tbChessRating ON wsUsers.GUID = tbChessRating.PlayerID")
		or die("Invalid query: " . mysql_error());

	echo '<table>';
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


</body>
</html>

