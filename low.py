import glob 
import os.path
files= glob.glob('cold/*.mli') + glob.glob('cold/*.ml')
# glob.glob('src/*.ml') + glob.glob('src/*.mli')

def translate(s):
    return s[0].lower() +s[1:]


for i in files:
    base,name=os.path.split(i)
    dst= os.path.join(base,translate(name))
    # if name[0].isupper():
    #     print name

    os.rename(i,dst)
