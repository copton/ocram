import gnuplot
import os

script = """
set terminal pngcairo size 1024, 1024
set output '%(outfile)s.png'
set key inside right top vertical Right noreverse noenhanced autotitles nobox

set style data linespoints
set palette gray

set auto x
set auto y

plot '%(infile)s' using 2:xtic(1) title columnheader(2), '' using 3 title columnheader(3), '' using 4 title columnheader(4)
"""

def plot(path, values, (apps_, variants_, measurements_)):
    measurements = ["text"]
    assert set(measurements).issubset(set(measurements_)), str(measurements_)

    apps = ["rpc1", "rpc2", "rpc3"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "gen"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    plotdata = gnuplot.linespoints(values, apps, variants, ["text"])

    config = {'outfile': os.path.join(path, "pt_text")}
    gnuplot.plot(script, config, plotdata)
