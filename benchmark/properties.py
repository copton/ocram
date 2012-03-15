from measurements import *

import os.path

def frac(a, b):
    return 100.0 * (b - a) / a

class Properties(object):
    def __init__(self, name):
        self.name = name
        for i in Properties.attributes():
            setattr(self, i, float('nan'))

    def proportion(self, rhs):
        prop = Properties("(%s)/(%s)" % (self.name, rhs.name))
        for i in Properties.attributes():
            setattr(prop, i, frac(getattr(self, i), getattr(rhs, i)))
        return prop

    def subtract(self, rhs):
        prop = Properties("(%s)-(%s)" % (self.name, rhs.name))
        for i in Properties.attributes():
            setattr(prop, i, getattr(self, i) - getattr(rhs, i))
        return prop

    @staticmethod
    def attributes():
        return ["text", "bss", "data", "loc", "stack", "cpu"]


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

    def native_properties(self, cfile, elf):
        return self.app_properties("nat", cfile, elf)

    def tcode_properties(self, cfile):
        prop = Properties("tc")
        prop.loc = lines_of_code(self.p(cfile))
        return prop

    def ecode_properties(self, ecfile, palfile, elf):
        prop = self.app_properties("ec", ecfile, elf)
        prop.loc += lines_of_code(self.p(palfile))
        return prop

    def pal_properties(self, cfile, elf):
        return self.app_properties("pal", cfile, elf, no_rti=True)

    def tl_properties(self, cfile, elf):
        return self.app_properties("tl", cfile, elf)

