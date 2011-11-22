my %arr;
my $stakes;

while ($line = <>) # read a file
{ 
    if($line =~ m/^Номер ставки: (\d+)/) # meet 
    {
		if($stakes != "")
		{
			$arr{$index} =  $stakes;
		}

		$index = $1;
		$stakes = ""
    }
	
    if($line =~ m/\d+\t\d+\.\d+\s\d+\:\d+.*\t(\S+)\t.*/) # meet 
    {
		$res = $1;
		if($res == 'X')
		{	
			$res = '0';
		}
		$stakes = $stakes.$res;
	}

	if (eof()) 
	{
		$arr{$index} =  $stakes;
	}
}

while (($num, $stake) = each(%arr))
{
	print "$num = $stake\n";
}

