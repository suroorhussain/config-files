#!/usr/bin/env python
import json
import sys

if __name__ == '__main__':
    data = sys.stdin.read()

    parsed = json.loads(data)

    print json.dumps(parsed, indent=2)
