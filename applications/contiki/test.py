#!/usr/bin/env python

from subprocess import Popen
import sys
import os

verify = sys.argv[1]
csc = sys.argv[2]
contiki = "/home/alex/scm/contiki"
cwd = os.getcwd()

os.environ["JAVA_HOME"] = "/opt/jdk1.6.0_30"
os.environ["PATH"] = "/opt/jdk1.6.0_30/bin:" + os.environ["PATH"]

code = Popen("schroot -c contiki -p -d %(cwd)s -- java -ea -jar %(contiki)s/tools/cooja/dist/cooja.jar -nogui=%(csc)s -contiki=%(contiki)s" % locals(), shell=True).wait()
assert code == 0, code

code = Popen("%(verify)s OcramCooja.log" % locals(), shell=True).wait()
assert code == 0, code
