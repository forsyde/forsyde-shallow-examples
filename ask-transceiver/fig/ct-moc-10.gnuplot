set xlabel "seconds" 
plot "./fig/ct-moc-sig_ct_lpout1.dat" with linespoints title "sig_ct_lpout"

set terminal postscript eps color
set output "fig/ct-moc-graph-sig_ct_lpout.eps"
replot 
set terminal epslatex color
set output "fig/ct-moc-graph-sig_ct_lpout-latex.eps"
replot
