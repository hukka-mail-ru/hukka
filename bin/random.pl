 #!/usr/bin/perl 
 
 @files = <*.mp3>;
 @numbers;

 foreach $file (@files) 
 {
	$arraySize = @files;

	for(;;)
	{
		$random = int(rand($arraySize));
		$found = 0;
		foreach $num (@numbers) 
	 	{
			if($num == $random)
			{
				$found = 1;
			}
		}


		if($found == 0)
		{
			push (@numbers, $random);
			goto END;
		}

	}
END:
    $newname = "file." . $random . "." . $file;
	rename($file, $newname);

 } 
