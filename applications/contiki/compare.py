#!/usr/bin/env python

import sys
import os
import re
import difflib

assert len(sys.argv) == 3
select = sys.argv[1]
assert select in ["trace", "observe"]
app_path = sys.argv[2]
assert os.path.isdir(app_path)

logline_pattern = re.compile(r'^log output: ([0-9]*): ([0-9]): ' + select + r': (.*)$')
sleep_pattern = re.compile(r'sleep: tics=([0-9]*)(, cond.waiting=.)?$')

def load_log(app_type):
    f = open(os.path.join(app_path, app_type, "OcramCooja.log"), "r")
    log = []
    for line in f.readlines():
        mo = logline_pattern.match(line)
        if mo:
            log.append(mo.group(3) + "\n")
    
    return log

def diff_log(native, other, fromfile, tofile):
    if native != other:
        sys.stdout.write("comparison (" + select + ") failed!\n")
        diff = difflib.unified_diff(native, other, fromfile=fromfile, tofile=tofile)
        for line in diff:
            sys.stdout.write(line)
        sys.exit(1)

log_generated = load_log("generated")
log_runtime = load_log("runtime")

if select == "trace":
    log_native = load_log("native")
    diff_log(log_native, log_generated, "native", "generated")
    diff_log(log_native, log_runtime, "native", "runtime")
else:
    if len(log_generated) != len(log_runtime):
        diff_log(log_generated, log_runtime, "observe")

    for i in range(len(log_generated)):
        tc = log_generated[i]
        tl = log_runtime[i]
        if tc != tl:
            mo_tc = sleep_pattern.match(tc)
            mo_tl = sleep_pattern.match(tl)
            if mo_tc != None and mo_tl != None and mo_tc.group(2) == mo_tl.group(2):
                sleep_tc = int(mo_tc.group(1))
                sleep_tl = int(mo_tl.group(1))
                if abs(sleep_tc - sleep_tl) <= 6:
                    continue
            diff_log(log_generated, log_runtime, "generated", "runtime")

sys.stdout.write("comparison (" + select + ") succeeded!\n")
