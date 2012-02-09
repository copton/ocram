#!/usr/bin/env python

import os
from os.path import join as pjoin
import sys
from subprocess import Popen, PIPE

from properties import *
import text
import properties

cwd=os.getcwd()
if not "benchmark" in os.listdir(cwd):
    sys.stderr.write("this script has to be executed from the top level project directory.\n")
    sys.exit(1)

sys.path.append(pjoin(cwd, "benchmark"))

ROOT = pjoin(cwd, "applications", "simulation_os")

def cleanup():
    sys.stderr.write("cleaning up\n")
    command = "cd %s; make clean" % ROOT
    code = Popen(command, shell=True, stdin=None).wait()
    assert code == 0, code

def load_setup(setup):
    sys.stderr.write("loading setup for %s\n" % setup)
    command = "cd %s; . ./setup-%s; echo $PLATFORM; echo $TOOLCHAIN" % (ROOT, setup)
    out, err = Popen(command, shell=True, stdout=PIPE, stderr=PIPE).communicate()
    if err:
        sys.stderr.write(err + "\n")
        sys.exit(1)
    platform, toolchain = tuple(out.split("\n")[:-1])

    sys.stderr.write("building for setup %s\n" % setup)
    command = "cd %s; . ./setup-%s; make" % (ROOT, setup)
    code = Popen(command, shell=True, stdin=None).wait()
    assert code == 0, code

    return platform, toolchain

def get_apps():
    apps = filter(lambda f: f != "os" and os.path.isdir(pjoin(ROOT, f)), os.listdir(ROOT))
    apps.sort()
    return apps

def get_setups():
    setups = map(lambda f: f.split("-")[1], filter(lambda f: f.startswith("setup-"), os.listdir(ROOT)))
    setups.sort()
    return setups

def measure(platform, toolchain, app):
    sys.stderr.write("running measurements for native %s application.\n" % app)
    native = app_properties("native", platform, toolchain, pjoin(ROOT, app, "native.c"), pjoin(ROOT, app, "native.elf"))
    sys.stderr.write("running measurements for T-code %s application\n" % app)
    tc = app_properties("tc", platform, toolchain, pjoin(ROOT, app, "tc.c"), pjoin(ROOT, app, "tc.elf"), no_rti=True)
    sys.stderr.write("running measurements for E-code %s application\n" % app)
    ec = app_properties("ec", platform, toolchain, pjoin(ROOT, app, "ec.c"), pjoin(ROOT, app, "ec.elf"))

    sys.stderr.write("processing...\n")
    pal = pal_properties(toolchain, pjoin(ROOT, app, "pal.c"), pjoin(ROOT, app, "pal.o"))
    overhead = get_overhead(native, tc, ec)
    normalized = get_normalized(native, ec, pal)

    text.print_all_properties([native, tc, ec, pal, overhead, normalized]) 

cleanup()
for setup in get_setups():
    platform, toolchain = load_setup(setup) 
    for app in get_apps():
        measure(platform, toolchain, app)
    cleanup()
