# coding=utf-8
import logging
import json


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
        """
        Parameter:
            filename: str, e.g. 'alert/subalerts/rules/apar_rules.json'

        Return:
            json format data, dict
        """
        with open(filename, "rb") as r:
            return json.load(r)

    @staticmethod
    def write_json(filename, data):
        with open(filename, "wb") as w:
            json.dump(data, w)


class ShowAnything(object):

    def __init__(self):
        # ignore
        self.perfix = [".", "PyX", "PyO", "bin", "lib", "include"]
        self.suffix = [".swp"]

    def list_obj(self, name):

        # m.lower().endswith(('.png', '.jpg', '.jpeg'))
        if name.lower().endswith('.json'): self.json_file(name)

    def json_file(self, name):
        try:
            with open(name, "rb") as r:
                data = json.load(r)
        except Exception, e:
            Util.logger.info(e)
            return None
        print data
        print json.dumps(data, indent=4, sort_keys=True)

    def folder(self):
        pass


