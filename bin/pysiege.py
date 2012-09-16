#!/usr/bin/python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import urllib2
from threading import Thread
from threading import BoundedSemaphore


def download(url, sem):
    try:
        r = urllib2.urlopen(url)
        r.read()
    finally:
        sem.release()

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Format HTML.')
    parser.add_argument('url', type=str, help="url")

    args = parser.parse_args()

    threads = []
    sem = BoundedSemaphore(100)
    count = 0
    while True:
        count += 1
        sem.acquire()
        t = Thread(target=download, args=(args.url, sem))
        t.daemon = True
        t.start()
        if not count % 200:
            print count
