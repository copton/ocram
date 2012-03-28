#!/usr/bin/env python

import sys
import os
import pkgutil
from os.path import join as pjoin

assert os.environ.has_key("ROOT")
root = os.environ["ROOT"]
sys.path.insert(0, pjoin(root, "benchmark"))

import text

croot = pjoin(root, "applications", "contiki")
sys.path.insert(0, croot)

import plots

data, meta = text.import_all_properties(croot, "bench.results")

for importer, name, ispkg in pkgutil.iter_modules(plots.__path__):
    module = __import__("plots." + name, fromlist=[name])
    module.plot(data, meta)
