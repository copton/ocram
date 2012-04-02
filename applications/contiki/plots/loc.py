import plotting
import StringIO
import os

script = """
set terminal pngcairo size 640, 480
set output '%(outfile)s.png'

set style data histogram
set style histogram cluster gap 1

set style fill pattern 1 border

set key left
set auto x
set yrange [0:*]

set ylabel "lines of code"

plot '%(infile)s' using 2:xtic(1) title col, '' using 3:xtic(1) title col, '' using 4:xtic(1) title col
"""

def plot(path, values, (apps_, variants_, measurements), numbers):
    assert "loc" in measurements
    
    apps = ["dca", "coap", "rpc2"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "gen"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    plotdata = plotting.grouped_boxes(values, apps, variants, "loc")

    config = {'outfile': os.path.join(path, "loc")}
    plotting.plot(script, config, plotdata)

    lst = []
    for a in apps:
        lst.append(plotting.frac(values[(a, "nat", "loc")], values[(a, "gen", "loc")]))

    numbers.write("savings: min: %.2f\n" % min(lst))
    numbers.write("savings: max: %.2f\n" % max(lst))
