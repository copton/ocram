#! /usr/bin/env python

import fileinput
import re
import sys

logline = re.compile(r'^([0-9]*)\tID:([0-9])\t(.*)$')
sensorinput = re.compile(r'^reading value from sensor: ([0-9]*)$')
netinput = re.compile(r'^.*: send values: (.*)$')
netoutupt = re.compile(r'^.*: received values: (.*)$')

values = []

def readLog(line):
    mo = logline.match(line.strip())
    assert mo, line
    time = int(mo.group(1))
    node = int(mo.group(2))
    text = mo.group(3)
    return time, node, text

def readValues(values):
    return map(lambda x: int(x), values.strip().split(" "))

def fail(valIs, valShould):
    sys.stderr.write("verification failed: should be %d but is %d\n" % (valShould, valIs))
    sys.exit(1)

for line in fileinput.input():
    time, node, text = readLog(line)
    if node == 1:
        mo = netinput.match(text)
        if not mo:
            continue
        sys.stdout.write(line)
        values += readValues(mo.group(1))

    elif node == 2:
        mo = sensorinput.match(text)
        if not mo:
            continue
        sys.stdout.write(line)
        values.append(int(mo.group(1)))

    elif node == 3:
        mo = netoutupt.match(text)
        if not mo:
            continue
        sys.stdout.write(line)
        [minIs, maxIs] = readValues(mo.group(1))
        minShould = min(values)
        maxShould = max(values)
        if minIs != minShould:
            fail(minIs, minShould)
        if maxIs != maxShould:
            fail(maxIs, maxShould)
        values = []
