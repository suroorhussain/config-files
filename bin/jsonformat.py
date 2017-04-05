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


def main(args):
    if args.fn:
        data = open(args.fn).read()
    else:
        data = sys.stdin.read()

    parsed = json.loads(data)
    #sort_object(parsed)
    clean_json = json.dumps(parsed, indent=2, sort_keys=True)
    if args.inplace:
        open(args.fn, 'w').write(clean_json)
    else:
        print clean_json


if __name__ == '__main__':
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument('fn', nargs='?')
    parser.add_argument('-i',
                        '--inplace',
                        action='store_true',
                        default=False)
    args = parser.parse_args()
    main(args)
