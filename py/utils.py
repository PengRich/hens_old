# coding=utf-8
from __future__ import unicode_literals
from treelib import Tree
import logging
import json
import os


LOG_LEVEL  = "debug"
LOG_OUTPUT = 0
'''log configuration'''
levels = {'debug': logging.DEBUG,
          'info': logging.INFO,
          'warnging': logging.WARNING,
          'error': logging.ERROR,
          'critical': logging.CRITICAL}

form = '[%(asctime)s | %(filename)s, %(funcName)s, '
form += 'line %(lineno)d | %(levelname)s]  %(message)s'
formatter = logging.Formatter(form)
logging.getLogger('hens').setLevel(levels["debug"])

if LOG_OUTPUT == 0:
    ch = logging.StreamHandler()
    ch.setLevel(levels[LOG_LEVEL])
    ch.setFormatter(formatter)
    logging.getLogger('hens').addHandler(ch)

if LOG_OUTPUT == 1:
    path = os.getcwd()
    fh   = logging.FileHandler(os.path.join(path, "log", "logfile.log"))
    fh.setLevel(levels[LOG_LEVEL])
    fh.setFormatter(formatter)
    logging.getLogger('hen').addHandler(fh)


class Util(object):

    logger = logging.getLogger("hens")

    def __init__(self):
        pass

    @staticmethod
    def read_json(filename):
        with open(filename, "rb") as r:
            return json.load(r)

    @staticmethod
    def write_json(filename, data):
        with open(filename, "wb") as w:
            json.dump(data, w)


class ShowAnything(object):

    def __init__(self):
        # ignore
        self.perfix = [".", "PyX", "PyO", "bin", "lib", "include", "__"]
        self.suffix = [".swp", ".pyc"]
        self.branch = 1
        self.data   = {}

    def _explore_data(self, data, parent):

        if isinstance(data, list):
            for i in data:
                self.branch += 1
                self.tree.create_node(i, self.branch, parent=parent)

        if isinstance(data, dict):
            for i, j in data.iteritems():
                self.tree.create_node(i, self.branch, parent=parent)
                self._explore_data(j, self.branch)
                self.branch += 1

    def list_obj(self, name):

        # m.lower().endswith(('.png', '.jpg', '.jpeg'))
        if name.lower().endswith('.json'): self.json_file(name)
        self.tree = Tree()
        self.tree.create_node(os.path.split(name)[-1].split(".")[0], 0)
        self._explore_data(self.data, 0)
        self.tree.show(line_type="ascii-emv")

    def json_file(self, name):

        try:
            data = Util.read_json(name)
        except Exception, e:
            Util.logger.info(e)
            return None

        del data["mod"]
        del data["mod_map_file"]
        del data["obj"]
        self.data = data
        # print json.dumps(data, indent=4, sort_keys=True)

    def folder(self, path):
        filenames = os.listdir(path)

        for i in self.perfix:
            filenames = [j for j in filenames if not j.startswith(i)]
        for i in self.suffix:
            filenames = [j for j in filenames if not j.endswith(i)]

        for name in filenames:
            self.data[name] = {}
            if os.path.isdir(os.path.join(path, name)):
                self.folder(os.path.join(path, name))
        # for name in sorted(filenames):
        #     data.append({"name": name, "parent": parent, "branch": branch})
        #     branch += 1
        #     if os.path.isdir(os.path.join(path, folder, name)):
        #         data, branch = \
        #             self.explore_folders(os.path.join(path, folder), name,
        #                                  branch-1, branch, data)

        # return data, branch


if __name__ == "__main__":
    ShowAnything().list_obj("json/debug.json")
