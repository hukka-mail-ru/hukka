<?php
	
	$DbServerAddr = 'localhost';
	$DbServerName = 'WapServer3';
	$DbServerPwd =  'win74';
	$DbName = 'WapServer3DB';
	
	$DbErrorConnect = 'Can\'t connect to DB (' . $DbName. '): ';
	$DbErrorSelect = 'Can\'t select DB (' . $DbName. '): ';


	function OpenPage($name)
	{
		printf("<html>\n");
		printf("<head>\n");
		printf("<title>%s</title>\n", $name);
		printf("<link rel='stylesheet' type='text/css' href='style/style.css' />\n");
		printf("</head>\n");
		printf("<meta http-equiv='content-type' content='text/html; charset=UTF-8'>\n");
		printf("<body class='wrapper'>\n");
	} 


	function ClosePage()
	{
		printf("</body>\n");
		printf("</html>\n");
	} 

?> 
