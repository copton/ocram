from subprocess import Popen, PIPE
import os
import re
import sys

def app_section_size(toolchain, elf):
    """
    input: path to elf file
    output: (text, data, bss)
    """
    out, err = Popen([toolchain+"size", elf], stdout=PIPE).communicate()
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
    if platform == "avrora":
        return max_stack_usage__avrora(elf)
    else:
        sys.stderr.write("warning: don't know how to measure stack usage for platform '%s'.\n" % platform)
        return -1

def max_stack_usage__avrora(elf):
    """
    input: path to elf file (build for avr)
    output: max stack size
    """
    os.putenv("CLASSPATH", "/home/alex/scm/avrora/bin/")
    out, err = Popen(["java", "avrora.Main", "-colors=false", "-monitors=stack", "-seconds=10", elf], stdout=PIPE).communicate()
    assert not err, err
    pattern = re.compile(r'Maximum stack size: ([0-9]*) bytes')
    for line in out.split("\n"):
        mo = pattern.match(line)
        if mo:
            return int(mo.group(1))
    assert False, out
