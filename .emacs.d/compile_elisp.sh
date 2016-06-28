#!/bin/bash

find ~/.emacs.d/ -iname "*.elc" -delete
emacs -batch -l ~/.emacs.d/compile-all-elisp.el
