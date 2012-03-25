#!/usr/bin/env python

import sys
from TOSSIM import *

t = Tossim([])
m = t.getNode(23)
m.bootAtTime(42)
t.addChannel("BlinkC", sys.stdout)

for i in xrange(10000):
    x = t.runNextEvent()
