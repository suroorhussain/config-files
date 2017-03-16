#!/usr/bin/env python2
import json
import sys


def sort_key(obj):
    if isinstance(obj, dict):
        return json.dumps(obj, indent=2, sort_keys=True)
    else:
        return obj


def sort_object(obj):
    if isinstance(obj, dict):
        for key, value in obj.iteritems():
            sort_object(value)
    elif isinstance(obj, list):
        for elem in obj:
            sort_object(elem)
        obj.sort(key=sort_key)


if __name__ == '__main__':
    data = sys.stdin.read()

    parsed = json.loads(data)
    #sort_object(parsed)
    print json.dumps(parsed, indent=2, sort_keys=True)
