#!/bin/sh

date=`date "+%Y-%m-%dT%H_%M_%S"`
HOME=/home/jvanwink/

rsync -azP \
  --delete \
  --delete-excluded \
  --exclude-from=$HOME/.rsync/exclude \
  --link-dest=../current \
  $HOME /media/backup_data/Backups/incomplete_back-$date \
  && mv /media/backup_data/Backups/incomplete_back-$date \
    /media/backup_data/Backups/back-$date \
  && rm -f /media/backup_data/Backups/current \
  && ln -s back-$date /media/backup_data/Backups/current"
