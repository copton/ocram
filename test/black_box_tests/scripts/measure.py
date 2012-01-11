#!/usr/bin/env python

import os
import sys
import re
from subprocess import Popen, PIPE

ROOT=os.environ["ROOT"]
TOOLCHAIN=os.environ["TOOLCHAIN"]

def app_section_size(elf):
    """
    input: path to elf file
    output: (text, data, bss)
    """
    out, err = Popen([TOOLCHAIN+"size", elf], stdout=PIPE).communicate()
    assert not err, err
    values = out.split("\n")[1].split()
    return tuple([int(v) for v in values[:3]])

def lines_of_code(cfile):
    """
    input: path to c file
    output: lines of code
    """
    f = open(cfile, "r")
    loc = len(f.read().split("\n"))
    f.close()
    return loc

def max_stack_usage(elf):
    """
    input: path to elf file (build for avr)
    output: max stack size
    """
    os.putenv("CLASSPATH", "/home/alex/scm/avrora/bin/")
    out, err = Popen(["java", "avrora.Main", "-colors=false", "-monitors=stack", "-seconds=10", elf], stdout=PIPE).communicate()
    assert not err, err
    pattern = re.compile(r'Maximum stack size: ([0-9]*) bytes')
    for line in out.split("\n"):
        mo = pattern.match(line)
        if mo:
            return int(mo.group(1))
    assert False, out

class Properties(object):
    def __init__(self, name):
        self.name = name

    def measure(self, no_stack=False):
        self.text, self.data, self.bss = app_section_size(self.name + ".elf")
        self.loc = lines_of_code(self.name + ".c")
        if no_stack:
            self.stack = -1
        else:
            self.stack = max_stack_usage(self.name + ".elf")

    @staticmethod
    def attributes():
        return ["text", "bss", "data", "loc", "stack"]

    @staticmethod
    def format():
        return "%10s\t%7s\t%7s\t%7s\t%7s\t%7s"

    @staticmethod
    def header():
        return Properties.format() % tuple([""] + Properties.attributes())

    def __repr__(self):
        return Properties.format() % (self.name, self.text, self.bss, self.data, self.loc, self.stack)
def properties(app):
    def frac(a, b):
        return "%.2f" % (100.0 * (b - a) / a)

    os.chdir(app)

    native = Properties("native")
    tc = Properties("tc")
    ec = Properties("ec")
    pal = Properties("pal")
    overhead = Properties("overhead")
    normalized = Properties("normalized")

    native.measure()
    tc.measure(no_stack=True)
    ec.measure()

    pal.text, pal.data, pal.bss = app_section_size("../../os/pal.o")
    pal.loc = lines_of_code("../../os/pal.c")
    pal.stack = -1

    overhead.text = frac(native.text, ec.text)
    overhead.bss = frac(native.bss, ec.bss)
    overhead.data = frac(native.data, ec.data)
    overhead.loc = frac(native.loc, tc.loc)
    overhead.stack = frac(native.stack, ec.stack)

    normalized.text = frac(native.text, ec.text - pal.text)
    normalized.bss = frac(native.bss, ec.bss - pal.bss)
    normalized.data = frac(native.data, ec.data - pal.data)
    normalized.loc = frac(native.loc, tc.loc - pal.loc)
    normalized.stack = -1

    sys.stdout.write("\n".join([Properties.header()] + [str(x) for x in [native, tc, ec, pal, overhead, normalized]]))
    sys.stdout.write("\n")
    
def get_apps():
    app_path = os.path.join(os.environ["ROOT"], "application")
    apps = filter(lambda f: os.path.isdir(f), map(lambda f: os.path.join(app_path, f), os.listdir(app_path)))
    apps.sort()
    return apps

for app in get_apps():
    properties(app)
