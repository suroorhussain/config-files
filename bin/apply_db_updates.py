#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import unicode_literals
import psycopg2
import glob


def run_file(fn, cur, record):
    with open(fn) as f:
        sql = f.read()
        print 'applying', fn
        cur.execute(sql)

    if record:
        cur.execute('INSERT INTO patches_applied (patch_fn) VALUES (%s)',
                    [fn])


def init_tracking(cur):
    cur.execute('CREATE TABLE patches_applied (patch_fn TEXT PRIMARY KEY);')

if __name__ == '__main__':
    import argparse

    parser = argparse.ArgumentParser(description='Apply db changes.')
    parser.add_argument('-d',
                        dest="dbname",
                        type=str,
                        help='dbname')
    parser.add_argument('-U',
                        dest='user',
                        type=str,
                        help='db user')
    parser.add_argument('--reload',
                        dest="reload_fn",
                        type=str,
                        help="reload specified file")
    parser.add_argument('--init',
                        default=False,
                        action='store_true',
                        help="init target db for schema tracking")
    parser.add_argument('sql_dir',
                        type=str,
                        help="directory holding sql deltas")

    args = parser.parse_args()

    con = psycopg2.connect(database=args.dbname, user=args.user)
    cur = con.cursor()

    if args.init:
        init_tracking(cur)

    if args.reload_fn:
        record = False
        files_to_apply = [args.reload_fn]
    else:
        record = True
        files = set(glob.glob('*.sql'))

        cur.execute('SELECT patch_fn FROM patches_applied')

        applied_files = set(r[0] for r in cur.fetchall())

        files_to_apply = list(files - applied_files)

        files_to_apply.sort(key=lambda fn: int(fn.split('_')[0]))

        if not files_to_apply:
            print 'no new changes'
        else:
            print 'about to apply:'
            for fn in files_to_apply:
                print '   ', fn

    for fn in files_to_apply:
        run_file(fn, cur, record)

    con.commit()
