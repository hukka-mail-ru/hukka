<html>
 <head>
  <title>PHP Test</title>
 </head>
 <body>

<h2>Hello!</h2>

Rating table:<br>
<br>

<?php

	$lnk = mysql_connect('localhost', 'WapServer3', 'win74')
		   or die ('Not connected : ' . mysql_error());

	mysql_select_db('WapServer3DB', $lnk) or die ('Can\'t use WapServer3DB : ' . mysql_error());

	$result = mysql_query("SELECT wsUsers.User, tbChessRating.Rating FROM wsUsers INNER JOIN tbChessRating ON wsUsers.GUID = tbChessRating.PlayerID ORDER by wsUsers.User")
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
If you don't see yourself in the table, please pay us some money and enjoy:)<br>

</body>
</html>

