#!/usr/bin/env python

import os
import pkgutil
import sys
from os.path import join as pjoin

assert os.environ.has_key("ROOT")
root = os.environ["ROOT"]
sys.path.insert(0, pjoin(root, "benchmark"))

import text

croot = pjoin(root, "applications", "contiki")
sys.path.insert(0, croot)

import plots

ppath = pjoin(croot, "plots")

data = text.import_all_properties(croot, "bench.results")

f = open(pjoin(ppath, "number.txt"), "w")
for importer, name, ispkg in pkgutil.iter_modules(plots.__path__):
    module = __import__("plots." + name, fromlist=[name])
    f.write("\n" + name + "\n" + ("=" * len(name)) + "\n")
    module.plot(ppath, *data, numbers=f)

f.close()
