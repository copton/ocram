import plotting
import StringIO
import os

script = """
set terminal pngcairo mono size 640, 480
set style fill pattern 1 border
set output '%(outfile)s.png'
         
set style data histograms
set style histogram columnstacked


set datafile missing '-'

set key right 
set auto x
set yrange [42000:47000]
set ylabel "text [bytes]"
set xlabel "dca                       coap                       rpc"

set xtics ("" 0, \\
           "nat" 1, "tl" 2, "gen" 3, "" 4, \\
           "nat" 5, "tl" 6, "gen" 7, "" 8, \\
           "nat" 9, "tl" 10, "gen" 11, "" 12)

plot for [i=2:13] '%(infile)s' using i, '' u 14:key(1)
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
                values[(a, v, "pal")] = '-'

    data = StringIO.StringIO()
    for m in ["text", "pal"]:
        vals = [values[(a, v, m)] for a in apps for v in variants]
        plotting.out(data, [m] + [0] + vals[0:3] + [0] + vals[3:6] + [0] + vals[6:9] + [0])
    plotdata = data.getvalue()

    config = {'outfile': os.path.join(path, "text")}
    plotting.plot(script, config, plotdata)

    lst_pal = []
    lst_nopal = []
    for a in apps:
        lst_nopal.append(plotting.frac(values[(a, "nat", "text")], values[(a, "gen", "text")]))
        lst_pal.append(plotting.frac(values[(a, "nat", "text")], values[(a, "gen", "text")] + values[(a, "gen", "pal")]))

    numbers.write("overhead: nopal: max: %.2f\n" % max(lst_nopal))
    numbers.write("overhead: pal: max: %.2f\n" % max(lst_pal))
