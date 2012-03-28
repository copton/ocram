import os
import sys
import pickle
from properties import Properties

formatString = '%20s\t%7s\t%7s\t%7s\t%7s\t%7s\t%7s'

header = formatString % tuple([""] + Properties.attributes())

def print_properties(prop):
    def pretty(value):
        if type(value) == int:
            return str(value)
        else:
            return "%.2f" % value
    ps = [pretty(getattr(prop, p)) for p in Properties.attributes()]
    return formatString % tuple([prop.name] + ps)

def print_all_properties(out, props):
    out.write("# ")
    out.write("\n# ".join([header] + [print_properties(x) for x in props]))
    out.write("\n")

def export_all_properties(out, app, props):
    data = { }
    for prop in props:
        for measurement in Properties.attributes():
            key = (app, prop.name, measurement)
            assert not data.has_key(key)
            data[key] = getattr(prop, measurement)

    pickle.dump(data, out)

def import_all_properties(path, result_file):
    data = {}
    for (dirpath, dirnames, filenames) in os.walk(path):
        if result_file in filenames:
            dirnames[:] = []
            fname = os.path.join(dirpath, result_file)
            f = open(fname, "r")

            data.update(pickle.loads("".join(filter(lambda l: not l.startswith('#'), f.readlines()))))

    meta = map(lambda s: set(s), zip(*data.keys()))
    return data, meta
