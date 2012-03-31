import plotting
import StringIO
import os

script = """
set terminal pngcairo size 1024, 1024
set output '%(outfile)s.png'
         
set style histogram rowstacked title  offset character 2, 0.25, 0
set style data histograms
set style fill pattern 1 border
set palette gray 

set auto x
set yrange [0:*]

plot newhistogram "dca", '%(infile)s' using 2:xtic(1) t col, '' u 3 t col, '' u 4 t col, newhistogram "coap", '' u 5:xtic(1) t col, '' u 6 t col, '' u 7 t col, newhistogram "rpc2", '' u 8:xtic(1) t col, '' u 9 t col, '' u 10 t col
"""

def plot(path, values, (apps_, variants_, measurements_), numbers):
    measurements = ["bss", "data", "stack"]
    assert set(measurements).issubset(measurements_), str(measurements_)

    apps = ["dca", "coap", "rpc2"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "gen"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    plotdata = plotting.stacked_grouped_boxes(values, apps, variants, measurements)
   
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
