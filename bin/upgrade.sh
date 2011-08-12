
rsync -av --delete /ssd/pluto/ /ssd/bak.pluto.bak/
apt-get upgrade
rsync -av --delete /ssd/bak.pluto.bak/ /ssd/pluto/
