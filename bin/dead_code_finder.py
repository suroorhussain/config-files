#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals

from collections import defaultdict
import os
import sys
import tokenize


class TokenCollector(object):
    def __init__(self):
        self.token_count = defaultdict(lambda: 0)
        self.token_location = dict()
        self.def_tokens = set([])

        self.last_token = None
        self.abs_fn = None

    def set_current_file(self, abs_fn):
        if self.abs_fn != abs_fn:
            self.last_token = None
        self.abs_fn = abs_fn

    def __call__(self, *args):
        token_type = args[0]
        token = args[1]
        if token.startswith('__') and token.endswith('__'):
            return
        if token_type == 1:
            if self.last_token in ('def', 'class'):
                self.def_tokens.add(token)
                self.token_location[token] = (self.abs_fn, args[2][0])
                if token not in self.token_count:
                    self.token_count[token] = 0
            else:
                self.token_count[token] += 1

            self.last_token = token


if __name__ == '__main__':
    tc = TokenCollector()
    search_dirs = sys.argv[1:]
    for code_dir in search_dirs:
        for root, dirs, files in os.walk(code_dir):
            for fn in files:
                ext = os.path.splitext(fn)
                if ext in ('jar', 'js', 'pyc', 'pyo'):
                    continue
                abs_fn = os.path.join(root, fn)
                tc.set_current_file(abs_fn)
                # make sure you shouldn't check for hidden files
                if abs_fn.endswith('.py'):
                    try:
                        tokenize.tokenize(open(abs_fn).readline, tc)
                    except (tokenize.TokenError, IndentationError):
                        print abs_fn, 'could not tokenize as python source'

    for def_token in sorted(tc.def_tokens,
                            key=lambda x: tc.token_location.get(x)):
        if def_token not in tc.token_count:
            print 'token not found by tokenizer: %s' % def_token
        elif tc.token_count[def_token] == 0:
            abs_fn, line = tc.token_location.get(def_token)
            print '{0}:{1}:'.format(abs_fn, line), '\t\t', def_token
