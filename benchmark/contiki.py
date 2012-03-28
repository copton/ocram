import hashlib
import os
import re
from collections import defaultdict

def open_elf(elf):
    f = open(elf, "r")
    checksum = hashlib.md5(f.read()).hexdigest()
    f.close()

    f = open(os.path.join(os.path.dirname(elf), "OcramCooja.log"), "r")
    f.readline()
    line = f.readline()
    mo = re.match(r"^md5 sum of ([^:]*): (.*)$", line)
    assert mo, line
    assert mo.group(1) == elf, (mo.group(1), elf)
    assert mo.group(2) == checksum

    return f

def max_stack_usage(elf):
    f = open_elf(elf)
    pattern = re.compile(r"^max stack: ([^:]*): ([0-9]*)$")
    total = 0
    for line in f.readlines():
        mo = pattern.match(line)
        if mo and mo.group(1) == "<<main>>":
            total += int(mo.group(2))

    return total

def cpu_usage(elf):
    f = open_elf(elf)
    pattern = re.compile(r"^cpu cycles: ([^:]*): ([0-9]*)$")

    total = 0;
    for line in f.readlines():
        mo = pattern.match(line)
        if mo:
            identifier = mo.group(1)
            cycles = int(mo.group(2))
            if not identifier.startswith("0x"):
                total += cycles

    return total
