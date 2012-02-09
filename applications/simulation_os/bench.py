#!/usr/bin/env python

import sys
import os
from os.path import join as pjoin

assert os.environ.has_key("ROOT")
sys.path.insert(0, pjoin(os.environ["ROOT"], "benchmark"))

from properties import *
import text
import measurements

assert len(sys.argv) == 2
app_path = sys.argv[1]
assert os.path.isdir(app_path)

platform = os.environ["PLATFORM"]
toolchain = os.environ["TOOLCHAIN"]

native = app_properties("native", platform, toolchain, pjoin(app_path, "native.c"), pjoin(app_path, "native.elf"))
tc = app_properties("tc", platform, toolchain, pjoin(app_path, "tc.c"), pjoin(app_path, "tc.elf"), no_rti=True)
ec = app_properties("ec", platform, toolchain, pjoin(app_path, "ec.c"), pjoin(app_path, "ec.elf"))

pal = pal_properties(toolchain, pjoin(app_path, "pal.c"), pjoin(app_path, "pal.o"))
overhead = get_overhead(native, tc, ec)
normalized = get_normalized(native, ec, pal)

f = open("bench.%s.results" % platform, "w")
text.print_all_properties(f, [native, tc, ec, pal, overhead, normalized]) 
f.close()
