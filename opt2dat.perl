#!/usr/bin/perl -w

while (<>) {
  chomp;
  if (/fitness\[INITIAL\]=([\d\.]+)/) {
    ($i,$val) = (1,sprintf("%.5f",$1));
  }
  elsif (/fitness\[i=\s*(\d+)\]=([\d\.]+)/) {
    ($i,$val) = ($1,$2);
  }
  else {
    next;
  }
  print "$i\t$val\n";
}
