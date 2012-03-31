from measurements import *
from plotting import frac

import os.path

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
        return ["loc", "text", "bss", "data", "stack", "cpu"]


class Setup(object):
    def __init__(self, platform, toolchain, basepath):
        self.platform = platform
        self.toolchain = toolchain
        self.basepath = basepath

    def p(self, f):
        return os.path.join(self.basepath, f)

    def app_properties(self, name, cfile, elf, no_rti=False, no_loc=False):
        prop = Properties(name)
        prop.text, prop.data, prop.bss = app_section_size(self.toolchain, self.p(elf))
        if not no_loc:
            prop.loc = lines_of_code(cfile)
        if not no_rti:
            prop.stack = max_stack_usage(self.platform, self.p(elf))
            prop.cpu = cpu_usage(self.platform, self.p(elf))
        return prop

    def native_properties(self, cfile, elf):
        return self.app_properties("nat", cfile, elf)

    def tcode_properties(self, tfile, ecfile, elf):
        prop = self.app_properties("gen", ecfile, elf, no_loc=True)
        prop.loc = lines_of_code(tfile)
        return prop

    def pal_properties(self, cfile, elf):
        return self.app_properties("pal", cfile, elf, no_rti=True, no_loc=True)

    def tl_properties(self, cfile, elf):
        return self.app_properties("tl", cfile, elf)

