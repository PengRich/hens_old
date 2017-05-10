#!/usr/bin/env python
# coding=utf-8
import argparse

from py.utils import Util, ShowAnything


class management(object):

    def __init__(self):
        self.sh = ShowAnything()

    def run(self):

        parser = argparse.ArgumentParser(description="Manage file")
        add    = parser.add_argument

        # list folders and json files
        add("-l", "--List", nargs="?", help="list folders and files")

        args   = parser.parse_args()
        if args.List: self.sh.list_obj(args.List)


if __name__ == "__main__":
    management().run()
