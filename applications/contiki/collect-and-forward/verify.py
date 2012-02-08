#! /usr/bin/env python

import fileinput
import re
import sys

logline = re.compile(r'^log output: ([0-9]*): ([0-9]): (.*)$')
sensorinput = re.compile(r'^reading value from sensor: ([0-9]*)$')
netinput = re.compile(r'^.*: send values: (.*)$')
netoutupt = re.compile(r'^.*: received values: [^:]*: (.*)$')

def readLog(line):
    mo = logline.match(line.strip())
    if mo:
        time = int(mo.group(1))
        node = int(mo.group(2))
        text = mo.group(3)
        return time, node, text
    else:
        return None

def readValues(values):
    return map(lambda x: int(x), values.strip().split(" "))

def fail(valIs, valShould):
    sys.stderr.write("verification failed: should be %d but is %d\n" % (valShould, valIs))
    sys.exit(1)

def verify():
    values = []
    deliveries = 0

    for line in lines.fileinput.input()
        log = readLog(line)
        if log:
            time, node, text = log
            if node == 1:
                mo = netinput.match(text)
                if not mo:
                    continue
                sys.stdout.write(line + "\n")
                values += readValues(mo.group(1))

            elif node == 2:
                mo = sensorinput.match(text)
                if not mo:
                    continue
                sys.stdout.write(line + "\n")
                values.append(int(mo.group(1)))

            elif node == 3:
                mo = netoutupt.match(text)
                if not mo:
                    continue
                sys.stdout.write(line + "\n")
                [minIs, maxIs] = readValues(mo.group(1))
                minShould = min(values)
                maxShould = max(values)
                if minIs != minShould:
                    fail(minIs, minShould)
                if maxIs != maxShould:
                    fail(maxIs, maxShould)
                values = []
                deliveries += 1

    if deliveries == 0:
        sys.stderr.write("verification failed. Not enough log data\n")
        sys.exit(1)
    else:
        sys.stderr.write("verification succeeded.\n")

verify()
