#!/usr/bin/env python
import simplejson
import sys

if __name__ == '__main__':
    data = sys.stdin.read()

    parsed = simplejson.loads(data)

    print simplejson.dumps(parsed, indent=2, sort_keys=True)
