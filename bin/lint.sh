#!/bin/bash

find $1 -iname *.py | xargs -n1 lintrunner.sh
