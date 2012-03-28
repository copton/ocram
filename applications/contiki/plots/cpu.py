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

def plot(data, (apps_, variants, measurements)):
    assert "cpu" in measurements
    apps = ["rpc2", "dca", "coap"]
    assert set(apps).issubset(set(apps_)), str(apps)
    plotdata = StringIO.StringIO()

    variants.remove("pal")
    gnuplot.out(plotdata, ["app"] + list(variants))
    for app in apps:
        gnuplot.out(plotdata, [app] + [data[(app, variant, "cpu")] for variant in variants])

    config = {'outfile': os.path.join(os.environ["ROOT"], "applications", "contiki", "plots", "cpu")}
    gnuplot.plot(script, config, plotdata.getvalue())
