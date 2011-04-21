<html>
 <head>
  <title>Привет</title>
 </head>

  <meta http-equiv="content-type" content="text/html; charset=UTF-8">

 <body>

<h2>Привет!</h2>

<?php
		session_start();

		include 'defines.php';

		$lnk = mysql_connect($DbServerAddr, $DbServerName, $DbServerPwd) or die ($DbErrorConnect . mysql_error());
		mysql_select_db($DbName, $lnk) or die ($DbErrorSelect . mysql_error());

		$result = mysql_query("SELECT Rating FROM tbChessRating 
							   WHERE PlayerID = " . $_SESSION['UserID']);
		                   
		$row = mysql_fetch_array($result, MYSQL_NUM);
		$rating = $row[0];

		printf("Привет, %s! Твой рейтинг = %s", $_SESSION['UserName'], $rating);


?>


</body>
</html>
