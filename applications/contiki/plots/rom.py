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
    assert "text" in measurements
    
    apps = ["dca", "coap", "rpc2"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "tc"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    plotdata = gnuplot.grouped_boxes(values, apps, variants, "text")

    config = {'outfile': os.path.join(path, "rom")}
    gnuplot.plot(script, config, plotdata)
