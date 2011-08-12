#!/bin/sh

rsync -av \
    --delete \
    --delete-excluded \
    --exclude=/dev \
    --exclude=/proc \
    --exclude=/sys \
    --exclude=/home/jvanwink/.cache \
    --exclude=/home/jvanwink/Dropbox \
    --exclude=/home/jvanwink/.dropbox \
    --exclude=/home/jvanwink/.* \
    --exclude=/home/jvanwink/VMs/ \
    --exclude=/media/ \
    / \
    /media/backup_data/ebay_desktop/
