#!/usr/bin/env python

import sys
import os
import tempfile
import subprocess

base = os.path.join(os.environ["ROOT"], "applications", "contiki")

template = """
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

config = {
        "cpu" : { },
        "loc" : { },
        "stack" : { },
        "text" : { },
        "bss" : { },
        "data" : { },
        }

def read_results(fname):
    f = open(fname, "r")
    cells = [[cell.strip() for cell in row.strip().split("\t")] for row in f.readlines()[0:5]]
    data = dict(map(lambda row: (row[0], row[1:]), cells[1:]))
    f.close()
    return cells[0], data

def out(f, row):
    f.write("\t".join(row) + "\n")

def get_data():
    values = {}
    apps = []
    measurements = None
    variants = None
    for (dirpath, dirnames, filenames) in os.walk(base):
        if "bench.results" in filenames:
            dirnames[:] = []

            app = os.path.basename(dirpath)
            measurements_ , values_ = read_results(os.path.join(dirpath, "bench.results"))

            apps.append(app)

            if measurements == None:
                measurements = measurements_
            else:
                assert measurements == measurements_, (measurements, measurements_)

            if variants == None:
                variants = values_.keys()
            else:
                assert variants == values_.keys()

            values.update(dict([((app, variant, measurement), values_[variant][i]) for variant in variants for (i, measurement) in enumerate(measurements)]))

    assert "pal" in variants
    variants.remove("pal")
    return apps, measurements, variants, values

class Tmpfile(object):
    def __init__(self):
        self.fd, self.name = tempfile.mkstemp()
        self.fh = os.fdopen(self.fd, "w")

    def write(self, data):
        self.fh.write(data)

    def close(self):
        self.fh.close()

    def __del__(self):
        os.unlink(self.name)

def make_plot(measurement, apps, measurements, variants, values):
    infile = Tmpfile()
    out(infile, ["app"] + variants)
    for app in apps:
        out(infile, [app] + [values[(app, variant, measurement)] for variant in variants])
    infile.close()

    script = Tmpfile()
    template_config = config[measurement].copy()
    template_config.update({
        "infile": infile.name,
        "outfile" : os.path.join(base, "plot", measurement)})
    script.write(template % template_config)
    script.close()

    proc = subprocess.Popen(["gnuplot", script.name])
    proc.communicate()
    assert proc.returncode == 0, proc.returncode

apps, measurements, variants, values = get_data()
for measurement in measurements:
    make_plot(measurement, apps, measurements, variants, values)
