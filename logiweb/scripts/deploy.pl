#!/usr/bin/perl
# Code Snipplet to search a file
# by www.thedigitalchaos.de
#
use strict;
use File::Copy;

 
# show no warnings about recursion (we know what we do Wink )
no warnings "recursion";
 
# specify the file you search here (in this example "access_log" ) :
my $file = "TAP2_Spezifikation-2_1.9.pdf";
 
# specify the directory where you want to start the search (in this example "/var" ) :
my $searchdir = "L:/backend/LW_SERVERAPP";

my $destdir = "E:/amest_dev/servers";
 
# Calling the Subroutine, which searches the File
my $i = 0;
readDirectory($searchdir, $file);
print("\n\n ----  $i files copied ----- \n\n");
 
# We need an Subroutine, which can be called on every sub-directory
sub readDirectory
{
     my $searchdir = shift;
     my $searchfile = shift;
 
     # a little bit output, in which directory the script
     # is searching at the moment (the following line is not necessary)
     # print "Searching in $searchdir \n";
 
     # Open and close the directory
     opendir DIR, $searchdir or die("An error occured: $!");
     my @files = readdir(DIR);
     closedir DIR;
 
     foreach my $currentFile (@files)
     {
          # In Unix/Linux we have the directorys "." and "..",
          # it's no good idea to scan these, so let them skip.
          next if $currentFile =~ /^\./;
 
          # Lets have a look, if the current "file" is the searched file,
          # else have a look, if the "file" is an directory,
          # and if its one, lets have a look, if the searched file is into it.
          if ( $currentFile =~ /.*exe/ && $searchdir =~ /Release/ )
          {
               # We found the right file, now we can do somthing with it,
               # in this case, we only print a text
			   my $modtime1 = (stat("$searchdir/$currentFile"))[9];
			   my $modtime2 = (stat("$destdir/$currentFile"))[9];
			   
			   if($modtime1 != $modtime2)
			   {
					print "Copy the file: $searchdir/$currentFile \n";
					copy("$searchdir/$currentFile", "$destdir/$currentFile") or die "Copy failed: $!";
					$i++;
			   }
			   else
			   {
					print("skipped.. \n");
			   }
          }
          if ( -d "$searchdir/$currentFile" && $searchdir !~ /.*0.*/)
          {
               # The Subroutine is calling hisself with the new parameters
               readDirectory("$searchdir/$currentFile", $searchfile);
          }
     }
}