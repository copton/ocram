import subprocess
import tempfile
import os

class Tmpfile(object):
    def __init__(self):
        self.fd, self.name = tempfile.mkstemp()
        self.fh = os.fdopen(self.fd, "w")

    def write(self, data):
        self.fh.write(data)

    def close(self):
        self.fh.close()

    def __del__(self):
        os.unlink(self.name)

def out(f, row):
    f.write("\t".join(map(lambda x: str(x), row)) + "\n")

def plot(script, config, data):
    datafile = Tmpfile()
    datafile.write(data)
    datafile.close()

    scriptfile = Tmpfile()
    config.update({'infile': datafile.name})
    scriptfile.write(script % config)
    scriptfile.close()

    proc = subprocess.Popen(["/home/alex/local/bin/gnuplot", scriptfile.name])
    proc.communicate()
    assert proc.returncode == 0, proc.returncode
