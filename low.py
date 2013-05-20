import os.path
files=glob.glob('src/*.ml') + glob.glob('src/*.mli') + glob.glob('cold/*.mli') + glob.glob('cold/*.ml')

def translate(s):
    return s[0].lower() +s[1:]


for i in files:
    base,name=os.path.split(i)
    dst= os.path.join(base,translate(name))
    os.rename(i,dst)
