#!/usr/bin/env python

import os
from subprocess import check_call, call
import glob
import re
import logging

logging.basicConfig()
log = logging.getLogger(__name__)

def multiple_replace(dict, text):
  # Create a regular expression  from the dictionary keys
  regex = re.compile("(%s)" % "|".join(dict.keys() # map(re.escape, dict.keys())
                                   ))

  # For each match, look-up corresponding value in dictionary
  def dash_repl(matchobj):
      # print type(matchobj)
      # for g in matchobj.groups():
      for k in dict.keys():
          # the outest group
          if re.match(k, matchobj.group(0)):
              return dict[k]
      raise Exception("impossible {text}".format(text=text))  
  return regex.sub(dash_repl, text)

replace_dict  = {
    r'\[[0-9]+,[0-9]+\+[0-9]+\]': '[]',
    r'ghost': ''
}

content_diff = []

for x in glob.glob('*.ml'):
    output1 = "{x}.fan.ref".format(x=x)
    output2 = "{x}.fan.noloc.ref".format(x=x)
    output3 = "{x}.ocaml.ref".format(x=x)
    output4 = "{x}.ocaml.noloc.ref".format(x=x)
    cmd = "fan -p dparsetree -o {output1} {x}".format(x=x ,output1=output1)
    cmd2 = "dparsetree.native  {x} > {output3}".format(x=x, output3 = output3)
    env = { "x" : x ,
             "output1" : output1,
             "output2" : output2,
             "output3" : output3,
             "output4" : output4,
            "cmd" : cmd,
            "cmd2" : cmd2}

    log.info( "processing {x}".format(**env) )

    log.debug( ">> {cmd}".format(**env))

    retcode = call(cmd, shell = True)
    if retcode != 0:
        log.warn ("processing {x} failed!!".format(**env))
        continue;

    log.debug (">> {cmd2}".format(**env))
    # print cmd2
    retcode = call(cmd2, shell = True)
    if retcode != 0:
        log.warn( "processing {x} ,  failed!!".format(**env))
        continue;

    with open(output1, 'r') as inp1, \
         open(output2, 'w') as o1 , \
         open(output3, 'r') as inp2,\
         open(output4, 'w') as o2 :
        o1.write(multiple_replace(replace_dict, inp1.read()))
        o2.write(multiple_replace(replace_dict, inp2.read()))
    retcode=call("diff -b {output2} {output4} > {x}.noloc.diff".format
               (output2=output2, output4=output4, x = x),
               shell = True)
    if retcode != 0:
        log.warn("{output2},is different from,{output4}".format(**env))
        content_diff.append(x)
    retcode = call("diff -b {output1} {output3} > {x}.diff".format
               (output1=output1, output3=output3, x = x),
               shell = True)
    if retcode != 0:
        log.info("{output1}, is different from, {output3}".format(**env))
    
print content_diff
