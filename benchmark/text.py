import sys
from properties import Properties

formatString = "%10s\t%7s\t%7s\t%7s\t%7s\t%7s\t%7s"

header = formatString % tuple([""] + Properties.attributes())

def print_properties(prop):
    return formatString % (prop.name, prop.text, prop.bss, prop.data, prop.loc, prop.stack, prop.cpu)

def print_all_properties(out, props):
    out.write("\n".join([header] + [print_properties(x) for x in props]))
    out.write("\n")
