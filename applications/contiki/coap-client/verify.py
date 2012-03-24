#! /usr/bin/env python

import sys
import fileinput
import re

logline_pattern = re.compile(r'^log output: ([0-9]*): ([0-9]): trace: (.*)$')
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

query_pattern = re.compile(r'^query random: len=([0-9]*)$')
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
        data = mo.group(1)
        return data 
    else:
        return None

rands = "123456789abcdefghikjlmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXZY"

def nextRand(y):
    return (8 * y + 5) % len(rands)

def genRand(salt, length):
    state = salt
    result = ""
    for i in range(length):
        state = nextRand(state)
        result += rands[state]
    return result

def compare_random_data(salt, length, data):
    assert length == len(data), (length, len(data))
    data_should = genRand(salt, length)
    assert data == data_should, (data_should, data)

def verify(logs):
    salt = None
    length = None
    data = None
    for line in logs:
        log = logline(line.strip())
        if log != None:
            time, node, text = log
            if node == 1:
                nsalt = newsalt(text)
                if nsalt != None:
                    sys.stdout.write(line)
                    if data != None:
                        assert data != ""
                        assert length != None
                        compare_random_data(salt, length, data)
                        length = None
                    salt = nsalt - 1

                qlength = query(text)
                if qlength:
                    sys.stdout.write(line)
                    assert length == None
                    length = qlength
                    data = ""

                random_bytes = response(text)
                if random_bytes != None:
                    sys.stdout.write(line)
                    assert salt != None
                    assert length != None
                    data += random_bytes

    sys.stdout.write("verification successfull\n")

if __name__ == '__main__':
    verify(fileinput.input())
