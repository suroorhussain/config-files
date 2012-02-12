#!/usr/bin/env python
import simplejson
import sys


def sort_key(obj):
    if isinstance(obj, dict):
        return simplejson.dumps(obj, indent=2, sort_keys=True)
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

    parsed = simplejson.loads(data)
    #sort_object(parsed)
    print simplejson.dumps(parsed, indent=2, sort_keys=True)
