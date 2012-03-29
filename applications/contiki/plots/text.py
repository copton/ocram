import gnuplot
import StringIO
import os

script = """
set terminal pngcairo size 1024, 1024
set output '%(outfile)s.png'
         
set style histogram rowstacked title  offset character 2, 0.25, 0
set style data histograms
set style fill pattern 1 border
set palette gray 
set datafile missing '-'

set auto x
set yrange [0:*]

plot newhistogram "dca", '%(infile)s' using 2:xtic(1) t col, '' u 3 t col, newhistogram "coap", '' u 4:xtic(1) t col, '' u 5 t col, newhistogram "rpc2", '' u 6:xtic(1) t col, '' u 7 t col
"""

def plot(path, values_, (apps_, variants_, measurements_)):
    assert "text" in measurements_
    
    apps = ["dca", "coap", "rpc2"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "tc", "pal"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    variants.remove("pal")
    measurements = ["text", "pal"]
    values = {}
    for a in apps:
        for v in variants:
            if v == "tc":
                values[(a, v, "text")] = str(values_[(a, v, "text")] - values_[(a, "pal", "text")])
                values[(a, v, "pal")] = str(values_[(a, "pal", "text")])
            else:
                values[(a, v, "text")] = str(values_[(a, v, "text")])
                values[(a, v, "pal")] = "-"

    plotdata = gnuplot.stacked_grouped_boxes(values, apps, variants, measurements)
    config = {'outfile': os.path.join(path, "text")}
    gnuplot.plot(script, config, plotdata)
