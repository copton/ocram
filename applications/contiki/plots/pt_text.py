import plotting
import os

script = """
set terminal pngcairo size 640, 480
set output '%(outfile)s.png'

set style data linespoints

set key left
set xrange [-0.5:3.5]
set auto y

set ylabel "text [byte]"
set xlabel "number of worker threads"

plot '%(infile)s' using 2:xtic(1) title columnheader(2), for [i=3:5] '' using i title columnheader(i)
"""

def plot(path, values_, (apps_, variants_, measurements_), numbers):
    measurements = ["text"]
    assert set(measurements).issubset(set(measurements_)), str(measurements_)

    apps = ["rpc1", "rpc2", "rpc3", "rpc4"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "gen"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    values = {}
    for a in apps:
        for v in variants:
            values[(a,v,"text")] = values_[(a,v,"text")]
        values[(a,"gen_nopal", "text")] = values_[(a,"gen","text")] - values_[(a, "pal", "text")]

    variants.append("gen_nopal")
    plotdata = plotting.linespoints(values, apps, variants, measurements)

    config = {'outfile': os.path.join(path, "pt_text")}
    plotting.plot(script, config, plotdata)

    for v in variants:
        slope = 1.0 * (values[("rpc4", v, "text")] - values[("rpc1", v, "text")]) / 3
        numbers.write("slope: %s: %.2f\n" % (v, slope))
