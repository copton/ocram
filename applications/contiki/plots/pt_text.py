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

plot '%(infile)s' using 2:xtic(1) title columnheader(2), '' using 3 title columnheader(3), '' using 4 title columnheader(4), '' using 5 title columnheader(5)
"""

def plot(path, values_, (apps_, variants_, measurements_)):
    measurements = ["text"]
    assert set(measurements).issubset(set(measurements_)), str(measurements_)

    apps = ["rpc1", "rpc2", "rpc3"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "gen"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    values = {}
    for a in apps:
        for v in variants:
            values[(a,v,"text")] = values_[(a,v,"text")]
        values[(a,"gen_nopal", "text")] = values_[(a,"gen","text")] - values_[(a, "pal", "text")]

    variants.append("gen_nopal")
    plotdata = gnuplot.linespoints(values, apps, variants, measurements)

    config = {'outfile': os.path.join(path, "pt_text")}
    gnuplot.plot(script, config, plotdata)
