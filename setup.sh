#!/bin/bash

for f in `find .config-files -maxdepth 1 -mindepth 1 -name '.*'` .config-files/bin; do
    ln -f -s -v $f ;
done

.emacs.d/compile_elisp.sh
