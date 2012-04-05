import plotting
import os
from plots import script_prolog

script = script_prolog + """
set style data linespoints
set pointsize 2.0

set key left
set xrange [-0.5:3.5]
set auto y

set ylabel "RAM [byte]"
set xlabel "number of worker threads"

plot '%(infile)s' using 2:xtic(1) title columnheader(2), for [i=3:4] '' using i title columnheader(i)
"""

def plot(path, values_, (apps_, variants_, measurements_), numbers):
    measurements = ["data", "bss", "stack"]
    assert set(measurements).issubset(set(measurements_)), str(measurements_)

    apps = ["rpc1", "rpc2", "rpc3", "rpc4"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "gen"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    values = {}
    for a in apps:
        for v in variants:
            values[(a[3], v, "ram")] = sum([values_[(a, v, m)] for m in ["data", "bss", "stack"]])

    apps = ["1", "2", "3", "4"]
    plotdata = plotting.linespoints(values, apps, variants, ["ram"])

    config = {'outfile': os.path.join(path, "pt_ram")}
    plotting.plot(script, config, plotdata)

    for v in variants:
        slope = 1.0 * (values[("4", v, "ram")] - values[("1", v, "ram")]) / 3
        numbers.write("slope: %s: %.2f\n" % (v, slope))
