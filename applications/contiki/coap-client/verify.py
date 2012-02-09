#! /usr/bin/env python

import sys
import fileinput
import re

logline_pattern = re.compile(r'^log output: ([0-9]*): ([0-9]): (.*)$')
def logline(line):
    mo = logline_pattern.match(line.strip())
    if mo:
        time = int(mo.group(1))
        node = int(mo.group(2))
        text = mo.group(3)
        return time, node, text
    else:
        return None

newsalt_pattern = re.compile(r'^setting salt: ([0-9]*)$')
def newsalt(text):
    mo = newsalt_pattern.match(text)
    if mo:
        salt = int(mo.group(1))
        return salt
    else:
        return None

query_pattern = re.compile(r'^query random: ([0-9]*)$')
def query(text):
    mo = query_pattern.match(text)
    if mo:
        length = int(mo.group(1))
        return length
    else:
        return None

response_pattern = re.compile(r'^coap response: 2\.05: (.*)$')
def response(text):
    mo = response_pattern.match(text)
    if mo:
        checksum = mo.group(1)
        return checksum
    else:
        return None

def compare_checksum(salt, length, checksum):
    rands = "123456789abcdefghikjlmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZY"

    def nextRand(y):
        return (8 * y + 5) % len(rands)
    
    assert length == len(checksum), (length, len(checksum))
    state = salt
    for i, c in enumerate(checksum):
        state = nextRand(state)
        assert rands[state] == c, (i, rands[state], c)

def verify(logs):
    salt = None
    length = None
    checksum = None
    for line in logs:
        log = logline(line.strip())
        if log != None:
            time, node, text = log
            if node == 3:
                nsalt = newsalt(text)
                if nsalt != None:
                    sys.stdout.write(line)
                    if checksum != None:
                        assert checksum != ""
                        assert length != None
                        compare_checksum(salt, length, checksum)
                        length = None
                    salt = nsalt

                qlength = query(text)
                if qlength:
                    sys.stdout.write(line)
                    assert length == None
                    length = qlength
                    checksum = ""

                checksum_bytes = response(text)
                if checksum_bytes != None:
                    sys.stdout.write(line)
                    assert salt != None
                    assert length != None
                    checksum += checksum_bytes


if __name__ == '__main__':
    verify(fileinput.input())
