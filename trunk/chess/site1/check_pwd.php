<html>
 <head>
  <title>Авторизация</title>
 </head>

  <meta http-equiv="content-type" content="text/html; charset=UTF-8">

 <body>

<?php


if (isset($_POST['login']))
 {
         $login = $_POST['login']; 
         if ($login == '') { unset($login);}
 }
 if (isset($_POST['pwd']))
 {
         $pwd=$_POST['pwd']; 
          if ($pwd =='') { unset($pwd);}
 }



  
 if (empty($login) or empty($pwd))
 {
        echo '<center>',"Заполни поля!", '<br>
       <a href="auth.php">Назад</a>
           </center>';
 }
 else
 {
		$lnk = mysql_connect('localhost', 'WapServer3', 'win74')
		   or die ('Not connected : ' . mysql_error());

     	mysql_select_db('WapServer3DB', $lnk) or die ('Can\'t use WapServer3DB : ' . mysql_error()); 

    	$result_login = mysql_query("SELECT * FROM wsUsers WHERE User = '" . $login . "'");
		$num_rows = mysql_num_rows($result_login);


		if($num_rows == 0)
        {
			echo '<center>',"Такой пользователь не зарегистрирован!", '<br><a href="auth.php">Назад</a><br><a href="reg.php">Регистрация</a> </center>';
        }
		else
		{		
			$result_pwd = mysql_query("SELECT * FROM wsUsers WHERE User = '" . $login . "' AND Password = '" . $pwd . "'");
   		    $num_rows = mysql_num_rows($result_pwd);
			if($num_rows == 0)
		    {
				echo '<center>',"Неправильный пароль!", '<br><a href="auth.php">Назад</a></center>';
		    }
			else
			{
				header('Location: welcome_user.php');
				exit;
			}
		}	
 }
 
?>



</body>
</html>
