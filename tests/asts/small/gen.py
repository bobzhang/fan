#!/usr/bin/env python

import os
from subprocess import check_call, call
import glob
import re

for x in glob.glob('*.ml'):
    output1 = "{x}.fan.ref".format(x=x)
    output2 = "{x}.fan.noloc.ref".format(x=x)
    output3 = "{x}.ocaml.ref".format(x=x)
    output4 = "{x}.ocaml.noloc.ref".format(x=x)

    cmd = "fan -p dparsetree -o {output1} {x}".format(x=x ,output1=output1)
    print cmd
    check_call(cmd, shell = True)
    cmd2 = "dparsetree.native  {x} > {output3}".format(x=x, output3 = output3)
    print cmd2
    check_call(cmd2, shell = True)
    with open(output1, 'r') as inp1, \
         open(output2, 'w') as o1 , \
         open(output3, 'r') as inp2,\
         open(output4, 'w') as o2 :
        o1.write(re.sub(r'\[[0-9]+,[0-9]+\+[0-9]+\]','[]', inp1.read()))
        o2.write(re.sub(r'\[[0-9]+,[0-9]+\+[0-9]+\]','[]', inp2.read()))
    call("diff {output2} {output4} > {x}.noloc.diff".format
               (output2=output2, output4=output4, x = x),
               shell = True)
    call("diff {output1} {output3} > {x}.diff".format
               (output1=output1, output3=output3, x = x),
               shell = True)
    
