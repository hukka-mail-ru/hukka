 #!/usr/bin/perl 
 
 @files = <*.mp3>;

 foreach $file (@files) 
 {
	    # class body starts
    if($file =~ m/^file\.\w+\.(.+)/)
    {
		rename($file, $1);
    }
 } 
