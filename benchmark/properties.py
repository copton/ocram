from measurements import *

class Properties(object):
    def __init__(self, name):
        self.name = name
        for i in Properties.attributes():
            setattr(self, i, -1)

    @staticmethod
    def attributes():
        return ["text", "bss", "data", "loc", "stack", "cpu"]

def app_properties(name, platform, toolchain, cfile, elf, no_rti = False):
    prop = Properties(name)
    prop.text, prop.data, prop.bss = app_section_size(toolchain, elf)
    prop.loc = lines_of_code(cfile)
    if not no_rti:
        prop.stack = max_stack_usage(platform, elf)
        prop.cpu = cpu_usage(platform, elf)
    return prop

def pal_properties(toolchain, cfile, elf):
    pal = Properties("pal")
    pal.text, pal.data, pal.bss = app_section_size(toolchain, elf)
    pal.loc = lines_of_code(cfile)
    pal.stack = -1
    return pal

def frac(a, b):
    if a == -1:
        return -1
    if b == -1:
        return -1
    return "%.2f" % (100.0 * (b - a) / a)

def get_overhead(native, tc, ec):
    overhead = Properties("overhead")

    overhead.text = frac(native.text, ec.text)
    overhead.bss = frac(native.bss, ec.bss)
    overhead.data = frac(native.data, ec.data)
    overhead.loc = frac(native.loc, tc.loc)
    overhead.stack = frac(native.stack, ec.stack)
    overhead.cpu = frac(native.cpu, ec.cpu)

    return overhead

def get_normalized(native, ec, pal):
    normalized = Properties("normalized")
    normalized.text = frac(native.text, ec.text - pal.text)
    normalized.bss = frac(native.bss, ec.bss - pal.bss)
    normalized.data = frac(native.data, ec.data - pal.data)

    return normalized
