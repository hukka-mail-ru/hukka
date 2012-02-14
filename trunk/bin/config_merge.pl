#!/usr/bin/perl

# Usage : perl unit.pl [filename.h]

for ( A, B ) { open $_, "<\l$_" or die "Can't open \l$_: $!\n"; }
@a = <A>; @b = <B>; 


print @a; 
print @b; 