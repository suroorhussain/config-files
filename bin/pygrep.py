#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import mmap
import re


def scan_file(pattern, fn):
    try:
        f = open(fn)
        fd = f.fileno()
    except:
        return

    try:
        mmap_f = mmap.mmap(fd, 0, prot=mmap.PROT_READ)

        for match in pattern.finditer(mmap_f):
            yield match
    finally:
        f.close()


if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Process some integers.')
    parser.add_argument('pattern', nargs=1, type=unicode, help='regex pattern')

    parser.add_argument('files',
                        nargs='*',
                        type=unicode,
                        help='files to search')

    args = parser.parse_args()

    pattern = re.compile(args.pattern[0], re.MULTILINE)

    if args.files:
        for fn in args.files:
            for match in scan_file(pattern, fn):
                print match.groups(0)
