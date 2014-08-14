#!/usr/bin/gnuplot --persist

#set grid
#set title 'Sine oscillator'
#set pm3d map
#splot 'lv-map.dat' matrix with pm3d

reset
f(u,v)=log(u)-u+2*log(v)-v
set xrange [0.1:5]
set yrange [0.1:7]
set isosample 250, 250
set table 'test.dat'
splot f(x,y)
unset table

set contour base
set cntrparam level incremental -10, 0.5, 10
unset surface
set table 'cont.dat'
splot f(x,y)
unset table

reset
set xrange [0.1:5]
set yrange [0.1:7]
#unset key
p 'cont.dat' w l lt -1 lw 1,\
  'sys1.dat' using 2:3 with line title 'Integrated by Euler method' linecolor 1,\
  'sys1.dat' using 4:5 with line title 'Integrated by Runge-Kutta method' linecolor 2, \
  'sys1.dat' using 6:7 with line title 'Integrated by Implicit Euler method' linecolor 3, \
  'sys2.dat' using 2:3 with line title 'Integrated by Symplectic Euler method' linecolor 4
#p 'test.dat' with image, 'cont.dat' w l lt -1 lw 1,\
#  'sys2.dat' using 2:3 with line title 'Integrated by Symplectic Euler method' linecolor 4
#
# 'lv.dat' using 2:3 with points notitle linecolor 1,\
#  'lv.dat' using 4:5 with points notitle linecolor 2,\
