
<?php
	session_start();

	// initialization
	$_SESSION['authorized'] = false;

	header('Location: welcome.php');
?> 


