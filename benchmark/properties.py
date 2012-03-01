from measurements import *

import os.path

class Properties(object):
    def __init__(self, name):
        self.name = name
        for i in Properties.attributes():
            setattr(self, i, -1)

    @staticmethod
    def attributes():
        return ["text", "bss", "data", "loc", "stack", "cpu"]

def frac(a, b):
    if a == -1:
        return -1
    if b == -1:
        return -1
    return "%.2f" % (100.0 * (b - a) / a)

class Setup(object):
    def __init__(self, platform, toolchain, basepath):
        self.platform = platform
        self.toolchain = toolchain
        self.basepath = basepath

    def p(self, f):
        return os.path.join(self.basepath, f)

    def app_properties(self, name, cfile, elf, no_rti = False):
        prop = Properties(name)
        prop.text, prop.data, prop.bss = app_section_size(self.toolchain, self.p(elf))
        prop.loc = lines_of_code(self.p(cfile))
        if not no_rti:
            prop.stack = max_stack_usage(self.platform, self.p(elf))
            prop.cpu = cpu_usage(self.platform, self.p(elf))
        return prop

    def native_properties(self, cfile, elf, no_rti = False):
        return self.app_properties("native", cfile, elf, no_rti)

    def tcode_properties(self, cfile):
        prop = Properties("tcode")
        prop.loc = lines_of_code(self.p(cfile))
        return prop

    def ecode_properties(self, ecfile, palfile, elf, no_rti = False):
        prop = self.app_properties("ecode", ecfile, elf, no_rti)
        prop.loc += lines_of_code(self.p(palfile))
        return prop

    def pal_properties(self, cfile, elf):
        pal = Properties("pal")
        pal.text, pal.data, pal.bss = app_section_size(self.toolchain, self.p(elf))
        pal.loc = lines_of_code(self.p(cfile))
        pal.stack = -1
        return pal

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
