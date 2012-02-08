#!/usr/bin/env python

import os
from os.path import join as pjoin
import sys
from subprocess import Popen

from properties import *
from measurements import lines_of_code
import text
import properties

cwd=os.getcwd()
if not "benchmark" in os.listdir(cwd):
    sys.stderr.write("this script has to be executed from the top level project directory.\n")
    sys.exit(1)

sys.path.append(pjoin(cwd, "benchmark"))

ROOT = pjoin(cwd, "applications", "contiki")

def log(text):
    sys.stdout.write(">>>>>>>>>>>>>>>>>>>>> ")
    sys.stdout.write(text)
    sys.stdout.write("\n")

def cleanup(app):
    log("cleaning up application '%s'" % app)
    command = "cd %s; make clean" % pjoin(ROOT, app)
    code = Popen(command, shell=True, stdin=None).wait()
    assert code == 0, code

def test(app):
    log("testing application '%s'" % app)
    command = "cd %s; make test" % pjoin(ROOT, app)
    code = Popen(command, shell=True, stdin=None).wait()
    assert code == 0, code

def get_apps():
    apps = filter(lambda f: not f in ["os", "coap-client"] and os.path.isdir(pjoin(ROOT, f)), os.listdir(ROOT))
    apps.sort()
    return apps

def measure(app):
    platform = "contiki"
    toolchain = "schroot -c contiki -- msp430-"
    npath = pjoin(ROOT, app, "native")
    gpath = pjoin(ROOT, app, "generated")

    native = app_properties("native", platform, toolchain, pjoin(npath, "app.c"), pjoin(npath, "app.sky"))
    tc = Properties("tc")
    tc.loc = lines_of_code(pjoin(gpath, "app-tc.c"))
    ec = app_properties("ec", platform, toolchain, pjoin(gpath, "app-ec.c"), pjoin(gpath, "pal.sky"))

    pal = pal_properties(toolchain, pjoin(gpath, "pal.c"), pjoin(gpath, "pal.co"))
    overhead = get_overhead(native, tc, ec)
    normalized = get_normalized(native, ec, pal)

    text.print_all_properties([native, tc, ec, pal, overhead, normalized]) 

for app in get_apps():
    if len(sys.argv) > 1 and sys.argv[1] == "-c":
        cleanup(app)
    test(app)
    measure(app)
