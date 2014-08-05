#!/usr/bin/gnuplot --persist

set grid
set title 'Sine oscillator'
plot 'sine.dat' using 1:2 with points notitle linecolor 1,\
     'sine.dat' using 1:2 with line title 'Integrated by Euler method' linecolor 1,\
     'sine.dat' using 1:3 with points notitle linecolor 2,\
     'sine.dat' using 1:3 with line title 'Integrated by Runge-Kutta method' linecolor 2
