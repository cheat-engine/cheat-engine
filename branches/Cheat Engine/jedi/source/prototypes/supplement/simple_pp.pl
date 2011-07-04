#!/usr/bin/perl
##############################################################################
##
## Simple perl script to filter only a single DEFINE symbol from the input
## file.
##
##############################################################################
my ($param1, $param2) = @ARGV;

if ((-z $param1) || ($param2 eq ''))
{
  print <DATA>;
  exit;
}
my $positive = 1;
# Open and read and close the file
open(FILE, $param1) or die;
$x = join('', <FILE>);
close(FILE);
# Check wether the symbol is negated or not
if ($param2 =~ /^!(.+)$/i)
{
  $param2 = $1;
  $positive = 0;
}
# According to the commandline evaluate the symbol
if ($positive != 0)
{
  # Replace all IFDEF ELSE ENDIF statements
  $x =~ s/\{\$IFDEF[\t\s]+$param2\}[\t\s\n]*(.+?)\{\$ELSE[\t\s]+[~]{0,1}$param2\}[\t\s\n]*(.+?)\{\$ENDIF[\t\s]+[~]{0,1}$param2\}[\t\s\n]*/$1/gism;
  $x =~ s/\{\$IFNDEF[\t\s]+$param2\}[\t\s\n]*(.+?)\{\$ELSE[\t\s]+[~]{0,1}$param2\}[\t\s\n]*(.+?)\{\$ENDIF[\t\s]+[~]{0,1}$param2\}[\t\s\n]*/$2/gism;
  # Replace all IFDEF ENDIF statements
  $x =~ s/\{\$IFDEF[\t\s]+$param2\}[\t\s\n]*(.+?)\{\$ENDIF[\t\s]+[~]{0,1}$param2\}[\t\s\n]*/$1/gism;
  $x =~ s/\{\$IFNDEF[\t\s]+$param2\}[\t\s\n]*(.+?)\{\$ENDIF[\t\s]+[~]{0,1}$param2\}[\t\s\n]*//gism;
}
else # If the symbol was negated at the commandline
{
  # Replace all IFDEF ELSE ENDIF statements
  $x =~ s/\{\$IFDEF[\t\s]+$param2\}[\t\s\n]*(.+?)\{\$ELSE[\t\s]+[~]{0,1}$param2\}[\t\s\n]*(.+?)\{\$ENDIF[\t\s]+[~]{0,1}$param2\}[\t\s\n]*/$2/gism;
  $x =~ s/\{\$IFNDEF[\t\s]+$param2\}[\t\s\n]*(.+?)\{\$ELSE[\t\s]+[~]{0,1}$param2\}[\t\s\n]*(.+?)\{\$ENDIF[\t\s]+[~]{0,1}$param2\}[\t\s\n]*/$1/gism;
  # Replace all IFDEF ENDIF statements
  $x =~ s/\{\$IFDEF[\t\s]+$param2\}[\t\s\n]*(.+?)\{\$ENDIF[\t\s]+[~]{0,1}$param2\}[\t\s\n]*//gism;
  $x =~ s/\{\$IFNDEF[\t\s]+$param2\}[\t\s\n]*(.+?)\{\$ENDIF[\t\s]+[~]{0,1}$param2\}[\t\s\n]*/$1/gism;
}
# Output to STDOUT
print "$x";
__END__
Syntax:

  simple_pp.pl <filename> [!]<symbol>

Output is printed to STDOUT. If you want to treat multiple symbols simply run
the script multiple times.

The exclamation mark in front of the symbol can be used to undefine the
respective symbol - otherwise it will be defined.

Any DEFINE or UNDEF directives concerning the given symbol inside the input
file will be ignored. Only commandline is taken.

