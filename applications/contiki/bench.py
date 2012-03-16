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

platform = "contiki"
toolchain = "schroot -c contiki -- msp430-"
setup = Setup(platform, toolchain, app_path)

def np(f):
    return os.path.join("native", f)

def gp(f):
    return os.path.join("generated", f)

def rp(f):
    return os.path.join("runtime", f)

nat = setup.native_properties(np("app.c"), np("app.cached.sky"))
tc = setup.tcode_properties(gp("app-tc.c"))
ec = setup.ecode_properties(gp("app-ec.c"), gp("pal.c"), gp("pal.cached.sky"))
pal = setup.pal_properties(gp("pal.c"), gp("pal.co"))
tl = setup.tl_properties(rp("app.c"), rp("pal.cached.sky"))

nat2tc = nat.proportion(tc)
nat2ec = nat.proportion(ec)
nat2ec_pal = nat.proportion(ec.subtract(pal))
ec2tl = ec.proportion(tl)

f = open("bench.results", "w")
text.print_all_properties(f, [nat, tc, ec, pal, nat2tc, nat2ec, nat2ec_pal, ec2tl])
f.close()
