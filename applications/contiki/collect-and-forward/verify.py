#! /usr/bin/env python

import fileinput
import re
import sys

logline_pattern = re.compile(r'^log output: ([0-9]*): ([0-9]): (.*)$')
def logline(text):
    mo = logline_pattern.match(text)
    if mo:
        time = int(mo.group(1))
        node = int(mo.group(2))
        text = mo.group(3)
        return time, node, text
    else:
        return None

sensorinput_pattern = re.compile(r'^reading value from sensor: ([0-9]*)$')
def sensorinput(text):
    mo = sensorinput_pattern.match(text)
    if mo:
        value = int(mo.group(1))
        return value
    else:
        return None

netinput_pattern = re.compile(r'^.*: send values: (.*)$')
def netinput(text):
    mo = netinput_pattern.match(text)
    if mo:
        values = map(lambda x: int(x), mo.group(1).strip().split(" "))
        return values
    else:
        return None

netoutput_pattern = re.compile(r'^.*: received values: [^:]*: (.*)$')
def netoutput(text):
    mo = netoutput_pattern.match(text)
    if mo:
        [minIs, maxIs] = map(lambda x: int(x), mo.group(1).strip().split(" "))  
        return (minIs, maxIs)
    else:
        return None

def fail(valIs, valShould):
    sys.stderr.write("verification failed: should be %d but is %d\n" % (valShould, valIs))
    sys.exit(1)

def verify(logs):
    values = []
    deliveries = 0

    for line in logs:
        log = logline(line.strip())
        if log != None:
            time, node, text = log
            if node == 1:
                vals = netinput(text)
                if vals != None:
                    sys.stdout.write(line)
                    values += vals

            elif node == 2:
                value = sensorinput(text)
                if value != None:
                    sys.stdout.write(line)
                    values.append(value)

            elif node == 3:
                stats = netoutput(text)
                if stats != None:
                    sys.stdout.write(line)
                    (minIs, maxIs) = stats
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

if __name__ == '__main__':
    verify(fileinput.input())
