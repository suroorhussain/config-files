#!/bin/bash

for f in \
    .bashrc \
    bin \
    .curlrc \
    .emacs.d \
    .fonts.conf \
    .gitconfig \
    .gtkrc \
    .gtkrc-2.0 \
    .hgrc \
    .profile \
    .sbclrc \
    .tidyrc \
    .xinitrc \
    .xmodmap \
    .Xmodmap \
    .xmonad \
    .Xresources \
    .zshrc ; do
    rm -f $f ;
    ln -s .config-files/$f ;
done
