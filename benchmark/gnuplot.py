import subprocess
import tempfile
import os
import StringIO

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

def stacked_grouped_boxes(values, apps, variants, measurements):
    plotdata = StringIO.StringIO()
    out(plotdata, ["variant"] + [m for a in apps for m in measurements])
    for v in variants:
        out(plotdata, [v] + [values[(a, v, m)] for a in apps for m in measurements])

    return plotdata.getvalue()

def grouped_boxes(values, apps, variants, measurement):
    plotdata = StringIO.StringIO()

    out(plotdata, ["app"] + variants)
    for app in apps:
        out(plotdata, [app] + [values[(app, variant, measurement)] for variant in variants])

    return plotdata.getvalue()

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
