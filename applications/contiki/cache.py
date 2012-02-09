#!/usr/bin/env python

import hashlib
import sys
import os

assert len(sys.argv) == 2


def gettarget(source):
    parts = source.split(".")
    parts.insert(-1, "cached")
    return ".".join(parts)

def gethash(name):
    md = hashlib.md5()
    try:
        f = open(name, "r")
    except IOError:
        return name
    md.update(f.read())
    f.close()
    return md.digest()

source = sys.argv[1]
target = gettarget(source)

shash = gethash(source)
thash = gethash(target)

if shash != thash:
    open(target, "w").write(open(source, "r").read())
