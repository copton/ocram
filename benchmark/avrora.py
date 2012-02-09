import os
import re
from subprocess import Popen, PIPE

def max_stack_usage(elf):
    os.putenv("CLASSPATH", "/home/alex/scm/avrora/bin/")
    out, err = Popen(["java", "avrora.Main", "-colors=false", "-monitors=stack", "-seconds=10", elf], stdout=PIPE).communicate()
    assert not err, err
    pattern = re.compile(r'Maximum stack size: ([0-9]*) bytes')
    for line in out.split("\n"):
        mo = pattern.match(line)
        if mo:
            return int(mo.group(1))
    assert False, out

