import plotting
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

def plot(path, values, (apps_, variants_, measurements), numbers):
    assert "cpu" in measurements
    
    apps = ["dca", "coap", "rpc2"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "gen"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    plotdata = plotting.grouped_boxes(values, apps, variants, "cpu")

    config = {'outfile': os.path.join(path, "cpu")}
    plotting.plot(script, config, plotdata)

    lst_tl = []
    lst_gen = []
    for a in apps:
        lst_tl.append(plotting.frac(values[(a, "nat", "cpu")], values[(a, "tl", "cpu")]))
        lst_gen.append(plotting.frac(values[(a, "nat", "cpu")], values[(a, "gen", "cpu")]))

    print apps, lst_tl
    numbers.write("overhead: tl: max: %.2f\n" % max(lst_tl))
    numbers.write("overhead: gen: max: %.2f\n" % max(lst_gen))
