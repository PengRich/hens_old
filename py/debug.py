# coding=utf-8
from py.utils import Util
import os
import commands


class Debug(object):

    def __init__(self):
        self.path = os.getcwd()

        self.name = os.path.join(self.path, "json", "debug.json")
        self.info = Util.read_json(self.name)
        self.mod_path = os.path.join("mod")
        self.inc_path = os.path.join("inc")

    def _mod_map_file(self, filenames):
        maps = {}
        for name in filenames:
            with open(name, "rb") as r:
                lines = r.readlines()
            for l in lines:
                if "module" in l and "end" not in l:
                    maps[l.replace("module ", "").strip()] = name
        self.info["mod_map_file"] = maps
        Util.write_json(self.name, self.info)

    def Compile(self, filenames):

        if filenames:
            self.info["compile"] = filenames
            Util.write_json(self.name, self.info)

        self._mod_map_file(self.info["compile"])

        Util.logger.info("Start Compile:")
        mod_path = os.path.join(self.path, "mod")
        inc_path = os.path.join(self.path, "inc")
        for f in self.info["compile"]:
            cmd = "gfortran -J "+self.mod_path+" -I "+self.inc_path+" -c "+f
            res = commands.getstatusoutput(cmd)
            Util.logger.info([res, cmd])

        self._mv_objs()

    def run(self, filename):

        objs = map(lambda x: os.path.join("obj", os.path.split(x)[-1]),
                   self.info["compile"])
        objs = " " + " ".join([i.replace("f90", "o") for i in objs]) + " "

        exc = os.path.split(filename)[-1].split(".")[0]
        cmd  = "gfortran -J "+self.mod_path+" -I "+self.inc_path+" "+\
            objs+filename+ " -o debug/"+exc
        res  = commands.getstatusoutput(cmd)
        Util.logger.info([res, cmd])
        cmd = "debug/" + exc
        Util.logger.info("RESULTS:")
        os.system(cmd)

    def _mv_objs(self):
        files = os.listdir(self.path)
        objs  = [f for f in files if f.endswith(".o")]
        if not objs: return None

        cmd = "mv " + " ".join(objs) + " " + os.path.join(self.path, "obj")
        Util.logger.info(cmd)
        res = commands.getstatusoutput(cmd)


if __name__ == "__main__":
    Debug()
