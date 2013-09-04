#!/usr/bin/python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals

import urllib2
from threading import Thread
from threading import Lock
from threading import BoundedSemaphore
from time import sleep

io_lock = Lock()

errors = 0
no_data = 0
success = 0
count = 0


def download(url, sem, sleep_time):
    global errors
    global no_data
    global success
    global count
    try:
        r = urllib2.urlopen(url)
        data = r.read()
        if len(data) == 0:
            with io_lock:
                no_data += 1
        else:
            success += 1
    except urllib2.HTTPError, e:
        with io_lock:
            errors += 1
            print '*' * 50
            print e.read()
            print '*' * 50
    finally:
        count += 1
        sleep(sleep_time)
        sem.release()

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Format HTML.')
    parser.add_argument('-c', '--concurrency',
                        type=int, default=1, help="concurrency")
    parser.add_argument('-s', '--sleep',
                        type=int, default=1, help="sleep time (seconds)")
    parser.add_argument('url', type=str, help="url")

    args = parser.parse_args()

    threads = []
    sem = BoundedSemaphore(args.concurrency)
    while True:
        sem.acquire()
        t = Thread(target=download, args=(args.url, sem, args.sleep))
        t.daemon = True
        t.start()
        if not count % 5 and count > 0:
            with io_lock:
                print '%s requests, %s success, %s errors, %s no-data' % (
                    count, success, errors, no_data)
