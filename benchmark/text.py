import sys
from properties import Properties

formatString = "%10s\t%7s\t%7s\t%7s\t%7s\t%7s"

header = formatString % tuple([""] + Properties.attributes())

def print_properties(prop):
    return formatString % (prop.name, prop.text, prop.bss, prop.data, prop.loc, prop.stack)

def print_all_properties(props):
    sys.stdout.write("\n".join([header] + [print_properties(x) for x in props]))
    sys.stdout.write("\n")

