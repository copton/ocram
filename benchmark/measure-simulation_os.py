#!/usr/bin/env python

import os
import sys
from subprocess import Popen, PIPE

from properties import *
import text
import properties

cwd=os.getcwd()
if not "benchmark" in os.listdir(cwd):
    sys.stderr.write("this script has to be executed from the top level project directory.\n")
    sys.exit(1)

sys.path.append(os.path.join(cwd, "benchmark"))

ROOT = os.path.join(cwd, "applications", "simulation_os")

def cleanup():
    command = "cd %s; make clean" % ROOT
    proc = Popen(command, shell=True, stdin=None, stdout=PIPE, stderr=PIPE)
    if proc.wait() != 0:
        sys.stderr.write(proc.stderr + "\n")
        sys.exit(1)

def load_setup(setup):
    sys.stdout.write("loading setup for %s\n" % setup)
    command = "cd %s; . ./setup/%s; echo $PLATFORM; echo $TOOLCHAIN" % (ROOT, setup)
    out, err = Popen(command, shell=True, stdout=PIPE, stderr=PIPE).communicate()
    if err:
        sys.stderr.write(err + "\n")
        sys.exit(1)
    platform, toolchain = tuple(out.split("\n")[:-1])

    command = "cd %s; . ./setup/%s; make" % (ROOT, setup)
    proc = Popen(command, shell=True, stdin=None, stdout=PIPE, stderr=PIPE)
    if proc.wait() != 0:
        sys.stderr.write(proc.stderr.read() + "\n")
        sys.exit(1)

    return platform, toolchain

def get_apps():
    app_path = os.path.join(ROOT, "application")
    apps = filter(lambda f: os.path.isdir(f), map(lambda f: os.path.join(app_path, f), os.listdir(app_path)))
    apps.sort()
    return apps

def get_setups():
    setup_path = os.path.join(ROOT, "setup")
    setups = filter(lambda f: f != "common", os.listdir(setup_path))
    setups.sort()
    return setups

def measure(platform, toolchain, app):
    native = app_properties("native", platform, toolchain, os.path.join(app, "native.c"), os.path.join(app, "native.elf"))
    tc = app_properties("tc", platform, toolchain, os.path.join(app, "tc.c"), os.path.join(app, "tc.elf"), no_stack=True)
    ec = app_properties("ec", platform, toolchain, os.path.join(app, "ec.c"), os.path.join(app, "ec.elf"))

    pal = pal_properties(toolchain, os.path.join(ROOT, "os", "pal.c"), os.path.join(ROOT, "os", "pal.o"))
    overhead = get_overhead(native, tc, ec)
    normalized = get_normalized(native, ec, pal)

    text.print_all_properties([native, tc, ec, pal, overhead, normalized]) 

cleanup()
for setup in get_setups():
    platform, toolchain = load_setup(setup) 
    for app in get_apps():
        measure(platform, toolchain, app)
    cleanup()
