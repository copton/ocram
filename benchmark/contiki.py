import hashlib
import os
import re

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
    pattern = re.compile(r"^max stack size: (.*)$")
    max_size = 0
    for line in f.readlines():
        mo = pattern.match(line)
        if mo:
            size = int(mo.group(1))
            if size > max_size:
                max_size = size

    return max_size

def cpu_usage(elf):
    f = open_elf(elf)
    thread_pattern = re.compile(r"^log output: [^:]*: [^:]*: thread address: ([^:]*): (.*)$")
    switch_pattern = re.compile(r"^cycle monitor action: ([^,]*), ([0-9]*)$")

    addresses = set()
    thread_ids = set()
    cycles = { }
    current_thread = None
    last_cycle_count = None
    for line in f.readlines():
        mo = thread_pattern.match(line)
        if mo:
            tid = int(mo.group(1))
            address = mo.group(2)
            assert not tid in thread_ids
            assert not address in addresses
            thread_ids.add(tid)
            addresses.add(address)
            
        mo = switch_pattern.match(line)
        if mo:
            address = mo.group(1)
            cycle_count = int(mo.group(2))
            if current_thread != None:
                cycles.setdefault(current_thread, 0)
                cycles[current_thread] += cycle_count - last_cycle_count
            last_cycle_count = cycle_count
            current_thread = address
    
    total = reduce(lambda x, y: x + y, [c for (a, c) in cycles.items() if a in addresses])
    return total
