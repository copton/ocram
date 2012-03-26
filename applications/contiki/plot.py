#! /usr/bin/env python

import sys
import os

base = os.path.join(os.environ["ROOT"], "applications", "contiki")

def read_results(fname):
    f = open(fname, "r")
    results = f.readlines()[0:5]
    f.close()
    return results

def row_out(row, f):
    for column in row.strip().split("\t"):
        cell = column.strip()
        f.write("," + cell)
    f.write("\n")

f = open(os.path.join(base, "plots", "input.csv"), "w")
header = False
for (dirpath, dirnames, filenames) in os.walk(base):
    if "bench.results" in filenames:
        dirnames[:] = []

        app = os.path.basename(dirpath)
        results = read_results(os.path.join(dirpath, "bench.results"))

        if header == False:
            f.write("application,variant")
            row_out(results[0], f)
            header = True

        for row in results[1:]:
            f.write(app)
            row_out(row, f)

f.close()
