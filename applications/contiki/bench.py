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

platform = "contiki"
toolchain = "schroot -c contiki -- msp430-"
npath = pjoin(app_path, "native")
gpath = pjoin(app_path, "generated")

native = app_properties("native", platform, toolchain, pjoin(npath, "app.c"), pjoin(npath, "app.cached.sky"))
tc = Properties("tc")
tc.loc = lines_of_code(pjoin(gpath, "app-tc.c"))
ec = app_properties("ec", platform, toolchain, pjoin(gpath, "app-ec.c"), pjoin(gpath, "pal.cached.sky"))

pal = pal_properties(toolchain, pjoin(gpath, "pal.c"), pjoin(gpath, "pal.co"))
overhead = get_overhead(native, tc, ec)
normalized = get_normalized(native, ec, pal)

f = open("bench.results", "w")
text.print_all_properties(f, [native, tc, ec, pal, overhead, normalized])
f.close()
