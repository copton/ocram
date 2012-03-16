import sys
from properties import Properties

formatString = '%20s\t%7s\t%7s\t%7s\t%7s\t%7s'

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
    out.write("\n".join([header] + [print_properties(x) for x in props]))
    out.write("\n")
