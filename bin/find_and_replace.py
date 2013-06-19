#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import argparse
import codecs
import re

parser = argparse.ArgumentParser()
parser.add_argument("-p", "--pattern", help="pattern")
parser.add_argument("-r", "--replace", help="replace with this")
parser.add_argument("filenames", nargs="+")
parser.add_argument("-d", "--dryrun", help="dryrun", action="store_true")


if __name__ == '__main__':
    args = parser.parse_args()
    pattern = re.compile(args.pattern)

    for fn in args.filenames:
        f = codecs.open(fn, 'r+', encoding="UTF-8")
        contents = f.read()
        new_contents = pattern.sub(args.replace, contents)
        if contents != new_contents:
            print fn
            if not args.dryrun:
                f.seek(0)
                f.truncate()
                f.write(new_contents)
