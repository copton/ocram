#!/usr/bin/env python

import jinja2
import itertools
import sys
import os

numberof_threads = len(sys.argv) - 1
assert numberof_threads > 0, "at leat one thread must be started"

thread_syscalls = map(lambda desc: desc.split(","), sys.argv[1:])
all_syscalls = set(itertools.chain(*thread_syscalls))
pal_code = sys.stdin.read()

def new_env():
    env = jinja2.Environment()
    env.block_start_string = r'/*{'
    env.block_end_string = r'}*/'
    env.variable_start_string = r'/*'
    env.variable_end_string = r'*/'
    env.comment_start_string = r'/*-'
    env.comment_end_stirng = r'-*/'
    return env

template = os.getenv("OCRAM_PAL_TEMPLATE")
sys.stdout.write(new_env().from_string(open(template, "r").read()).render(locals()))

