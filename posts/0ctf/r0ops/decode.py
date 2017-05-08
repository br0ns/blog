from idaapi import *
from idautils import *
from idc import *
import sys, os

# IDA does something to fd 1 and 2
def info(s):
    os.write(42, '%s\n' % s)

rsp = 0xe0b08a0
program = []
while True:
    addr = Qword(rsp)
    line = '0x%08x -> ' % rsp
    rsp += 8
    # Looks like all gadgets start on a jump
    jmp = DecodeInstruction(addr)
    # Not a pointer: The "stack" is probably exhausted
    if jmp is None:
        break
    assert jmp.get_canon_mnem() == 'jmp'
    addr = jmp.Op1.addr
    while True:
        size = MakeCode(addr)
        assert size > 0
        mnem = GetMnem(addr)
        line += '0x%08x: %s' % (addr, GetDisasm(addr))
        if   mnem == 'retn':
            break
        elif mnem == 'pop':
            line += ' ;; %d' % Qword(rsp)
            rsp += 8
        addr += size
        program.append(line)
        line = ' ' * 14

info('\n'.join(program))

exit()
