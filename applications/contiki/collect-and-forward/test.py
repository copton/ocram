#!/usr/bin/env python

import subprocess
from verify import verify
import sys
import os

csc = sys.argv[1]
contiki = "/home/alex/scm/contiki"

os.environ["JAVA_HOME"] = "/opt/jdk1.6.0_30"
os.environ["PATH"] = "/opt/jdk1.6.0_30/bin:" + os.environ["PATH"]

subprocess.Popen("schroot -c contiki -p -d /tmp -- java -jar %(contiki)s/tools/cooja/dist/cooja.jar -nogui=%(csc)s -contiki=%(contiki)s" % locals(), shell=True).communicate()

verify(open("/tmp/COOJA.testlog"))
