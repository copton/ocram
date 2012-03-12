#!/usr/bin/env python

import sys
import os
from os.path import join as pjoin

assert os.environ.has_key("ROOT")
sys.path.insert(0, pjoin(os.environ["ROOT"], "benchmark"))

from properties import *
import text

assert len(sys.argv) == 2
app_path = sys.argv[1]
assert os.path.isdir(app_path)

platform = os.environ["PLATFORM"]
toolchain = os.environ["TOOLCHAIN"]
setup = Setup(platform, toolchain, app_path)

native = setup.native_properties("native.c", "native.elf")
tc = setup.tcode_properties("tc.c")
ec = setup.ecode_properties("ec.c", "pal.c", "ec.elf")
pal = setup.pal_properties("pal.c", "pal.o")
overhead = get_overhead(native, tc, ec)
normalized = get_normalized(native, ec, pal)

f = open("bench.%s.results" % platform, "w")
text.print_all_properties(f, [native, tc, ec, pal, overhead, normalized]) 
f.close()
