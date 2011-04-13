#!/usr/bin/env python
import re
import sys
import time

_line_re = re.compile(r'postgres\[\d+\]:.?\[\d+\-\d+\](.*)')


def test_re():
    line = ('Sep 29 14:16:34 stub postgres[31489]: [36-3]'
            '	FROM pluto_dev.merchant_addresses')

    assert _line_re.search(line).group(1) == \
        '	FROM pluto_dev.merchant_addresses'


if __name__ == '__main__':
    while True:
        line = sys.stdin.readline()
        if line == '':
            time.sleep(.1)
        else:
            m = _line_re.search(line)
            if m:
                print m.group(1)
            else:
                print line
