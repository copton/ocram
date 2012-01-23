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
    proc = Popen(command, shell=True, stdin=None, stdout=PIPE, stderr=PIPE)
    if proc.wait() != 0:
        sys.stderr.write(proc.stderr.read() + "\n")
        sys.exit(1)

def load_setup(setup):
    sys.stderr.write("loading setup for %s\n" % setup)
    command = "cd %s; . ./setup/%s; echo $PLATFORM; echo $TOOLCHAIN" % (ROOT, setup)
    out, err = Popen(command, shell=True, stdout=PIPE, stderr=PIPE).communicate()
    if err:
        sys.stderr.write(err + "\n")
        sys.exit(1)
    platform, toolchain = tuple(out.split("\n")[:-1])

    sys.stderr.write("building for setup %s\n" % setup)
    command = "cd %s; . ./setup/%s; make" % (ROOT, setup)
    proc = Popen(command, shell=True, stdin=None, stdout=PIPE, stderr=PIPE)
    if proc.wait() != 0:
        sys.stderr.write(proc.stderr.read() + "\n")
        sys.exit(1)

    return platform, toolchain

def get_apps():
    app_path = pjoin(ROOT, "application")
    apps = filter(lambda f: os.path.isdir(f), map(lambda f: pjoin(app_path, f), os.listdir(app_path)))
    apps.sort()
    return apps

def get_setups():
    setup_path = pjoin(ROOT, "setup")
    setups = filter(lambda f: f != "common", os.listdir(setup_path))
    setups.sort()
    return setups

def measure(platform, toolchain, app):
    sys.stderr.write("running measurements for native application.\n")
    native = app_properties("native", platform, toolchain, pjoin(app, "native.c"), pjoin(app, "native.elf"))
    sys.stderr.write("running measurements for T-code application\n")
    tc = app_properties("tc", platform, toolchain, pjoin(app, "tc.c"), pjoin(app, "tc.elf"), no_stack=True)
    sys.stderr.write("running measurements for E-code application\n")
    ec = app_properties("ec", platform, toolchain, pjoin(app, "ec.c"), pjoin(app, "ec.elf"))

    sys.stderr.write("processing...\n")
    pal = pal_properties(toolchain, pjoin(ROOT, "os", "pal.c"), pjoin(ROOT, "os", "pal.o"))
    overhead = get_overhead(native, tc, ec)
    normalized = get_normalized(native, ec, pal)

    text.print_all_properties([native, tc, ec, pal, overhead, normalized]) 

cleanup()
for setup in get_setups():
    platform, toolchain = load_setup(setup) 
    for app in get_apps():
        measure(platform, toolchain, app)
    cleanup()
