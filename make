#!/usr/bin/env python
#vim: set filetype=python

import subprocess
import sys
import os

if len(sys.argv) != 2:
    sys.stderr.write("usage: make binary\n")
    sys.exit(1)

def execute(commandline):
    proc = subprocess.Popen(commandline.split(" "), stdout=subprocess.PIPE, stderr=subprocess.PIPE)
    out, err = proc.communicate()
    sys.stdout.write(out)
    sys.stdout.write(err)
    return (proc.returncode, out, err)

def notify(text, color):
    cmd = "/usr/bin/osd_cat -A right -p bottom -o -40 -d 2 -c %s" % color

    if os.fork() == 0:
        subprocess.Popen(cmd.split(' '), stdin=subprocess.PIPE).communicate(text)
        sys.exit(0)

def getFirstLine(text, predicate):
    return filter(predicate, text.split("\n"))[0]

def isBuildError(line):
    return line.startswith("src/") or line.startswith("test/")

def isTestError(line):
    return "[Failed]" in line

def diff_code(text):
    expected = []
    got = []

    for line in text.split("\n"):
        if line.startswith('expected: "') and line.count('"') == 2:
            (pre, code, post) = line.split('"')
            expected.append(code)
        if line.startswith(' but got: "') and line.count('"') == 2:
            (pre, code, post) = line.split('"')
            got.append(code)
            
    if len(expected) != len(got):
        sys.stderr.write("warning: expected != got: " + str((len(expected), len(got))) + "\n")

    def paste(name, code):
        f = open(name, "w")
        f.write(code.replace(r'\n', '\n'))
        f.close()

    inf = "/tmp/infile.c"
    outf = "/tmp/outfile.c"

    for (ins, outs) in zip(got, expected):
        print "----- diff"
        paste(inf, ins)
        paste(outf, outs)
        
        proc = subprocess.Popen(["wdiff", inf, outf])
        proc.wait()

(code, out, err) = execute("cabal-dev install")
if code != 0:
    open("./.quickfix", "w").write(err)
    notify(getFirstLine(err, isBuildError), "red")
    sys.exit(1)

(code, out, err) = execute("./cabal-dev/bin/%s --test --plain --hide-successes -j 3" % sys.argv[1])
if code != 0:
    notify(getFirstLine(out, isTestError), "red")
    diff_code(out)
    sys.exit(1)

notify("success", "green")
sys.exit(0)       
