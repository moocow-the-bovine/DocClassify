## -*- Mode: Shell-Script -*-

set xlabel "Category ID";
set ylabel "% Docs";

plot "train.dat" using ($1-.2):4:(0):4 with errorbars, \
     "test.dat"  using ($1+.0):4:(0):4 with errorbars, \
     "out.dat"   using ($1+.2):4:(0):4 with errorbars;