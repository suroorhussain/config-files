apt-get update
rsync -av --delete /ssd/pluto/ /ssd/bak.pluto.bak/
apt-get dist-upgrade
rsync -av --delete /ssd/bak.pluto.bak/ /ssd/pluto/
