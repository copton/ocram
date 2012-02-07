from subprocess import Popen, PIPE
import os
import re
import sys
import hashlib

def app_section_size(toolchain, elf):
    """
    input: path to elf file
    output: (text, data, bss)
    """
    out, err = Popen(toolchain+"size " + elf, shell=True, stdout=PIPE).communicate()
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

def max_stack_usage(platform, elf):
    """
    input:
            platform: the platform for which we do the measuremnts
            elf: path to elf file
    output: max stack size
    """
    if platform == "avrora":
        return max_stack_usage__avrora(elf)
    elif platform == "contiki":
        return max_stack_usage__contiki(elf)
    else:
        sys.stderr.write("warning: don't know how to measure stack usage for platform '%s'.\n" % platform)
        return -1

def max_stack_usage__contiki(elf):
    f = open(elf, "r")
    checksum = hashlib.md5(f.read()).hexdigest()
    f.close()

    f = open(os.path.join(os.path.dirname(elf), "test.log"), "r")
    f.readline()
    line = f.readline()
    mo = re.match(r"^md5 sum of ([^:]*): (.*)$", line)
    assert mo, line
    assert mo.group(1) == elf, (mo.group(1), elf)
    assert mo.group(2) == checksum

    pattern = re.compile(r"^max stack size: (.*)$")
    max_size = 0
    for line in f.readlines():
        mo = pattern.match(line)
        if mo:
            size = int(mo.group(1))
            if size > max_size:
                max_size = size

    return max_size

def max_stack_usage__avrora(elf):
    os.putenv("CLASSPATH", "/home/alex/scm/avrora/bin/")
    out, err = Popen(["java", "avrora.Main", "-colors=false", "-monitors=stack", "-seconds=10", elf], stdout=PIPE).communicate()
    assert not err, err
    pattern = re.compile(r'Maximum stack size: ([0-9]*) bytes')
    for line in out.split("\n"):
        mo = pattern.match(line)
        if mo:
            return int(mo.group(1))
    assert False, out
