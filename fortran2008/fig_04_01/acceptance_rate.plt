#!/usr/bin/gnuplot -persist
#
#    
#    	G N U P L O T
#    	Version 5.4 patchlevel 2    last modified 2021-06-01 
#    
#    	Copyright (C) 1986-1993, 1998, 2004, 2007-2021
#    	Thomas Williams, Colin Kelley and many others
#    
#    	gnuplot home:     http://www.gnuplot.info
#    	faq, bugs, etc:   type "help FAQ"
#    	immediate help:   type "help"  (plot window: hit 'h')

reset session

set terminal svg size 600,480 fixed enhanced standalone font 'Arial,12' butt dashlength 1.0 
set output 'acceptance_rate.svg'

set logscale x 10

set format x '{10}^{%L}'
set format y '%.2f'

set yrange [:1.01]

set xlabel 'the number of accepted and rejected samples'
set ylabel 'acception rate'


plot 'acceptance_rate.dat' \
     binary format="%1int%1double" \
     using 1:2 \
     with linespoints \
     notitle

set output

#    EOF
