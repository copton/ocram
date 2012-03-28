import gnuplot
import StringIO
import os

script = """
set terminal png
set output '%(outfile)s.png'

set style data histogram
set style histogram cluster gap 1
set palette gray

set style fill pattern 1 border
set auto x
set yrange [0:*]
plot '%(infile)s' using 2:xtic(1) title col, '' using 3:xtic(1) title col, '' using 4:xtic(1) title col
"""

def plot(path, values, (apps_, variants_, measurements)):
    return

