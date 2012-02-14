#!/usr/bin/perl

# Usage : perl logging.pl [filename.h]


while ($line = <>) # read a file
{ 
    if($line =~ m/(.*)\"A.Phys:(Flow|Test):([a-zA-Z0-9_]+)\" *, *(.*)/)
    {
        $begin = $1;
       $context = $2;
       $class = $3;
       $text = $4;

       if($context == "Flow") 
       { 
           $context = "FLOW_CONTEXT"; 
       } 
       if($context == "Test") 
       { 
           $context = "TEXT_CONTEXT"; 
       } 
        
       if($text =~ m/\"([a-zA-Z0-9_]+\:\:[a-zA-Z0-9_().,:;]+ +(.*))/)
      # if($text =~ m/\"([a-zA-Z0-9_]+\:\:.+ +(.*))/)
       {
          $out = $1;
          $message = $2;

          $line = $begin . $context . ", ADDITIONAL_DUMP << \"" . $message . "\n";
       }
       else
       {
          $line = $begin . $context . ", ADDITIONAL_DUMP << " . $text . "\n";
       }
    
    }

    print $line;
}

