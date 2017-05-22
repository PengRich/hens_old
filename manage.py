#!/usr/bin/env python
# coding=utf-8
import argparse

from py.utils import Util, ShowAnything
from py.debug import Debug


class management(object):

    def __init__(self):
        self.sh = ShowAnything()
        self.db = Debug()

    def run(self):

        parser = argparse.ArgumentParser(description="Manage file")
        add    = parser.add_argument

        # list folders and json files
        add("-l", "--List", nargs="?", help="list folders and files")

        # compile
        add("-c", "--Compile", nargs="*", help="comile source code")
        # debug
        add("-d", "--Debug", help="debug")
        # list debug logic
        add("-L", "--Logic", nargs="?", const=True, default=False,
            help="list compile logic")

        args   = parser.parse_args()
        # if args.List: self.sh.list_obj(args.List)

        if isinstance(args.Compile, list): self.db.Compile(args.Compile)
        if args.Debug: self.db.run(args.Debug)

        if args.Logic: self.sh.list_obj("json/debug.json")


if __name__ == "__main__":
    management().run()
    # d = Debug().run("workspace/example.f90")
