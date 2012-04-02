import plotting
import StringIO
import os

script = """
set terminal pngcairo size 640, 480
set style fill pattern 1 border
set output '%(outfile)s.png'
         
set style data histograms
set style histogram columnstacked

set key right
set auto x
set yrange [7000:9500]
set ylabel "RAM [bytes]"
set xlabel "dca                       coap                       rpc"

set xtics ("" 0, \\
           "nat" 1, "tl" 2, "gen" 3, "" 4, \\
           "nat" 5, "tl" 6, "gen" 7, "" 8, \\
           "nat" 9, "tl" 10, "gen" 11, "" 12)

plot for [i=2:13] '%(infile)s' using i, '' u 14:key(1)
"""

def plot(path, values, (apps_, variants_, measurements_), numbers):
    measurements = ["bss", "data", "stack"]
    assert set(measurements).issubset(measurements_), str(measurements_)

    apps = ["dca", "coap", "rpc2"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "gen"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    data = StringIO.StringIO()
    for m in measurements:
        vals = [values[(a, v, m)] for a in apps for v in variants]
        plotting.out(data, [m] + [0] + vals[0:3] + [0] + vals[3:6] + [0] + vals[6:9] + [0])
    plotdata = data.getvalue()

    config = {'outfile': os.path.join(path, "ram")}
    plotting.plot(script, config, plotdata)

    lst_bss = []
    lst_ram = []
    for a in apps:
        lst_bss.append(plotting.frac(values[(a, "nat", "bss")], values[(a, "gen", "bss")]))
        lst_ram.append(plotting.frac(
            sum([values[(a, "nat", m)] for m in measurements]),
            sum([values[(a, "gen", m)] for m in measurements])))

    numbers.write("overhead: bss: max: %.2f\n" % max(lst_bss))
    numbers.write("overhead: ram: max: %.2f\n" % max(lst_ram))
