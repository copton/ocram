import plotting
import StringIO
import os

script = """
set terminal pngcairo mono size 640, 480
set output '%(outfile)s.png'
         
set style data histograms
set style histogram rowstacked title

set style fill pattern 1 border

set datafile missing '0'

set key left
set auto x
set yrange [0:*]

set ylabel "text [bytes]"

plot newhistogram "dca", '%(infile)s' using 2:xtic(1) t col, '' u 3 t col, \\
     newhistogram "coap", '' u 4:xtic(1) t col, '' u 5 t col, \\
     newhistogram "rpc2", '' u 6:xtic(1) t col, '' u 7 t col
"""

def plot(path, values_, (apps_, variants_, measurements_), numbers):
    assert "text" in measurements_
    
    apps = ["dca", "coap", "rpc2"]
    assert set(apps).issubset(set(apps_)), str(apps_)

    variants = ["nat", "tl", "gen", "pal"]
    assert set(variants).issubset(set(variants_)), str(variants_)

    variants.remove("pal")
    measurements = ["text", "pal"]
    values = {}
    for a in apps:
        for v in variants:
            if v == "gen":
                values[(a, v, "text")] = values_[(a, v, "text")] - values_[(a, "pal", "text")]
                values[(a, v, "pal")] = values_[(a, "pal", "text")]
            else:
                values[(a, v, "text")] = values_[(a, v, "text")]
                values[(a, v, "pal")] = "-"

    plotdata = plotting.stacked_grouped_boxes(values, apps, variants, measurements)
    config = {'outfile': os.path.join(path, "text")}
    plotting.plot(script, config, plotdata)

    lst_pal = []
    lst_nopal = []
    for a in apps:
        lst_nopal.append(plotting.frac(values[(a, "nat", "text")], values[(a, "gen", "text")]))
        lst_pal.append(plotting.frac(values[(a, "nat", "text")], values[(a, "gen", "text")] + values[(a, "gen", "pal")]))

    numbers.write("overhead: nopal: max: %.2f\n" % max(lst_nopal))
    numbers.write("overhead: pal: max: %.2f\n" % max(lst_pal))
