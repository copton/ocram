#!/usr/bin/env python

import sys
import os
import re
import difflib

assert len(sys.argv) == 2
app_path = sys.argv[1]
assert os.path.isdir(app_path)

logline_pattern = re.compile(r'^log output: ([0-9]*): ([0-9]): trace: (.*)$')

def load_log(app_type):
    f = open(os.path.join(app_path, app_type, "OcramCooja.log"), "r")
    log = []
    for line in f.readlines():
        mo = logline_pattern.match(line)
        if mo:
            log.append(mo.group(3) + "\n")
    
    return log

log_native = load_log("native")
log_generated = load_log("generated")

if log_native != log_generated:
    sys.stdout.write("comparison failed!\n")
    diff = difflib.unified_diff(log_native, log_generated, fromfile="native", tofile="generated")
    for line in diff:
        sys.stdout.write(line)
    sys.exit(1)

sys.stdout.write("comparison succeeded!\n")
