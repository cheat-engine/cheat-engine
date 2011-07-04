# An AWK script to generate an .IMP file from
# the output of dumpbin -EXPORTS
BEGIN { # first find the list of exports
  do { getline } while ($1 != "ordinal");
  getline;
  print "UC1 MS"
}

$1 == "Summary" { exit }

{ print $1,$4  }
  