from subprocess import Popen, PIPE
import os
import re
import sys
import hashlib
import contiki
import avrora

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
        return avrora.max_stack_usage(elf)
    elif platform == "contiki":
        return contiki.max_stack_usage(elf)
    else:
        sys.stderr.write("warning: don't know how to measure stack usage for platform '%s'.\n" % platform)
        return -1


