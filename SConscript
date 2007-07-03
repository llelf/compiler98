import os

Import("env")
Import("hsenv")

badEnding = ["NplusK.hs","XML.hs","Setup.hs","Make2.hs"
            ,"GcodeFix.hs","GcodeLowC.hs","GcodeMem.hs","GcodeOpt1.hs"
            ,"GcodeOpt2.hs","GcodeRel.hs","GcodeSpec.hs","MainNhc98.hs"
            ,"_darcs","hugs","Floats.hs","Core" + os.sep + "Reduce.hs"]

yhcfiles = []
for root, dir, files in list(os.walk(".")):
    for f in files:
        skip = False
        for i in badEnding:
            if (root+os.sep+f).find(i) != -1:
                skip = True

        if skip:
            pass
        elif f.endswith("MainYhc.hs"):
            yhcfiles.append(hsenv.HaskellObject(root + os.sep + f, HSFLAGS=["""-cpp -DyhcVERSION=\\"%s\\" """ % (env["ENV"]["VERSION"], )]))
        elif f.endswith(".hs") or f.endswith(".lhs"):
            yhcfiles.append(hsenv.HaskellObject(root + os.sep + f))

Return("yhcfiles")
