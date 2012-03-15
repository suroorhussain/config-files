#!/usr/bin/env python

# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import re


# LogFormat "%a/%S %X %t \"%r\" %s/%>s %{pid}P/%{tid}P %T/%D %I/%O/%B" blackbox
blackbox_re = re.compile(
    r'^(?P<ip_address>\S+) '
    r'(?P<keep_alive>[+-X]) '
    r'\[(?P<time>[^\]]+)\] '
    r'"(?P<method>\w*) (?P<url>\S+) (?P<http_version>.*?)" '
    r'(?P<pre_status>\d+) (?P<status>\d+) '
    r'(?P<pid>\d+) (?P<tid>\d+) '
    r'(?P<seconds>-?\d+) (?P<microseconds>-?\d+) '
    r'(?P<in_bytes>\d+) (?P<out_bytes>\d+) (?P<body_bytes>\d+)$')


if __name__ == '__main__':
    import argparse
    import sys

    parser = argparse.ArgumentParser(description='Fully load solr.')
    parser.add_argument('filename', type=str, help='log file name')
    parser.add_argument('fields', type=str, nargs='+', help='fields to echo')
    args = parser.parse_args()

    with open(args.filename) as f:
        for line in f:
            m = blackbox_re.match(line)
            if m:
                result = []
                gd = m.groupdict()
                for field in args.fields:
                    result.append(gd[field])
                print ' '.join(result)
            else:
                print >> sys.stderr, line
